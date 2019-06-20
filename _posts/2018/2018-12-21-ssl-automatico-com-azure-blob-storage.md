---
layout: post
title: Usando Azure Blob Storage como armazenamento de certificados com Go acme/autocert
lang: pt
tags: [go, azure, autocert]
description: Por padrão, acme/autocert armazena os certificados em diretório local. Sendo assim, requisições subsequentes irão utilizar o mesmo certificado ao invés de obter um novo. Mas e se sua aplicação estiver executando em vários servidores? Como você garante que todos os servidores estão utilizando o mesmo certificado? Descubra aqui como podemos utilizar o Azure Blob Storage para resolver este problema.
ref: automated-ssl-with-azure-blob-storage
---

Se você não conhece o pacote `acme/autocert`, recomendo começar pelo artigo [Certificados SSL/TLS Gratuitos e Automatizados com Go](https://goenning.net/2017/11/08/certificados-gratuitos-automaticos-com-go/) para entender como utilizar este pacote para obter certificados SSL/TLS de graça e sem tarefas manuais.

Por padrão, acme/autocert armazena os certificados em diretório local. Sendo assim, requisições subsequentes irão utilizar o mesmo certificado ao invés de obter um novo.

O problema é que muitas aplicações estão hospedadas em um cluster, onde a aplicação web está executando em vários servidores ao mesmo tempo. Ter um cache local neste caso não é muito útil, pois cada servidor terá teu próprio cache. É possível usar NFS para resolver isto, mas existem melhores opções hoje em dia.

Este pacote permite que façamos a troca do mecanismo de cache através de uma implementação de interface, sendo assim, podemos escolher como e onde armazenar nossos certificados.

### Azure Blob Storage

[github.com/goenning/azcertcache](https://github.com/goenning/azcertcache) é um pacote Go que implementa uma estrátegia de Cache que armazena os certificados em um container do [Azure Blob Storage](https://azure.microsoft.com/en-us/services/storage/blobs/).

Para usar este pacote você precisa de um Account Name e Account Key que podem ser obtidos no Portal do Azure.

![](/public/images/2018/12/azbs-key.png)

E isto é tudo o que você precisa colocar no seu código:

```go
containerName := "autocertcache"
cache, err := azcertcache.New("<account name>", "<account key>", containerName)
if err != nil {
  // Handle error
}

m := autocert.Manager{
  Prompt:     autocert.AcceptTOS,
  Cache:      cache, // <-- this used to be autocert.DirCache("<folder name>"),
}

s := &http.Server{
  Addr:      ":https",
  TLSConfig: &tls.Config{GetCertificate: m.GetCertificate},
}

s.ListenAndServeTLS("", "")
```

Ta-da! 🎉

O processo funciona assim:

1. O primeiro request ao seu website meusite.com.br é recebido no servidor
2. autocert verifica se meusite.com.br se este certificado está no cache em memória, se estiver, o certificado é retornado ao cliente
3. caso não encontre, autocert então verifica se o certificado de meusite.com.br está no Azure Blob Storage, se estiver, o certificado é retornado ao cliente
4. caso não encontre, autocert obtem um novo certificado através do Let's Encrypt e armazena tanto no cache em memória quanto no Azure Blob Storage
5. Durante a próxima requisição, mesmo que um servidor diferente esteja encarregado de processá-la, o certificao já vai estar no Azure Blob Storage, pronto para ser reutilizado

**NOTE:** o cache em memória é perdido quando o processo é reiniciado, por isto é tão importante armazenar os certificados em algum local que seja resiliente

### BONUS!

Se você não utiliza Azure na sua infraestrutura, aqui estão algumas outras opções:

What if you're not using Azure Blob Storage? Well then you still have at least three other options:

1. [https://github.com/goenning/sqlcertcache](https://github.com/goenning/sqlcertcache) para armazenar os certificados em um banco de dados SQL
2. [https://github.com/danilobuerger/autocert-s3-cache](https://github.com/danilobuerger/autocert-s3-cache) para armazenar os certificados em algum serviço compatível com S3: AWS S3, DigitalOcean Spaces, Minio
3. [NFS](https://en.wikipedia.org/wiki/Network_File_System) e continue a usar `autocert.DirCache`.

Um abraço!