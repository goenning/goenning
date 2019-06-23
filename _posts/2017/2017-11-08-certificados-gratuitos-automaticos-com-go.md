---
layout: post
title: Certificados SSL/TLS Gratuitos e Automatizados com Go
lang: pt
tags: [go, segurança]
description: HTTPS se tornou obrigatório hoje em dia. Não só por questões de segurança, mas também pelo fato de que o Google da prioridade aos sites que estão acessíveis através de um protocolo seguro. Estamos em 2017 e já passou o tempo de que custo era uma desculpa aceitável para não ter HTTPS em nosso websites. Aprenda aqui como gerar certificados SSL/TLS de forma automática e sem gastar um centavo.
ref: free-and-automated-ssl-certificates-with-go
---

***Jan 13, 2018**: Este post foi atualizado para usar o desafio de HTTP, pois o Let's Encrypt desabilitou o desafio de TLS-SNI que era usado anteriormente.*

HTTPS se tornou obrigatório hoje em dia. Não só por questões de segurança, mas também pelo fato de que o Google da prioridade aos sites que estão acessíveis através de um protocolo seguro.

Estamos em 2017 e já passou o tempo de que custo era uma desculpa aceitável para não ter HTTPS em nosso websites.

Neste post vou mostrar como criar uma aplicação web em Go que automaticamente gera um certificado SSL/TLS em tempo de execução. O melhor de tudo é que **é de graça!**.

## Requerimentos

Se você quer acompanhar este demo, você precisará de:

- O compilador do Go;
- Um servidor que esteja disponível na internet; *Caso você não tenha nenhum, eu recomendo uma Máquina Virtual da [Digital Ocean](https://www.digitalocean.com/). Use por 24 horas e pague apenas $0.15.*
- Um domínio e acesso às configurações de DNS. Você não precisa disto se seu provedor disponibiliza um nomé público para você, por exemplo `meuservidor0001.meucloud.net`.

## Let’s Encrypt e o protocolo ACME

[Let’s Encrypt](https://letsencrypt.org/) é um emissor de certificados SSL/TLS que é bastante conhecida e aceita pela grande maioria dos navegadores. É possível emitir um certificado em menos de um segundo sem qualquer processo de registro ou pagamento.

**Autocert** é um pacote Go que implementa um cliente do protocolo ACME que é utilizado para comunicação com o Let’s Encrypt. Este pacote é a única dependência externa que você precisará, nenhuma outra instalação é necessária.

Você consegue obter este pacote utilizando o comando a seguir.

```
go get golang.org/x/crypto/acme/autocert
```

*Aqueles que procuram maiores informações sobre o protocolo ACME e outros pacotes alternativos, eu recomendo esta palestra (Inglês) [dotGo 2016 - Matthew Holt - Go with ACME](https://www.youtube.com/watch?v=KdX51QJWQTA)*

## O código ~~mágico~~ explicado passo à passo

```golang
package main

import (
	"crypto/tls"
	"fmt"
	"net/http"

	"golang.org/x/crypto/acme/autocert"
)

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello Secure World")
	})

	certManager := autocert.Manager{
		Prompt: autocert.AcceptTOS,
		Cache:  autocert.DirCache("certs"),
	}

	server := &http.Server{
		Addr:    ":443",
		Handler: mux,
		TLSConfig: &tls.Config{
			GetCertificate: certManager.GetCertificate,
		},
	}

	go http.ListenAndServe(":80", certManager.HTTPHandler(nil))
	server.ListenAndServeTLS("", "")
}
```

Começamos a função `main` criando um `mux` com uma simples mensagem de Hello World no caminho `/`. Neste exemplo estamos usando o Mux padrão do Go, mas poderia ser qualquer outro oferecido por pacotes externos.

No próximo passo criamos uma instância do `autocert.Manager`. Esta estrutura é responsável pela comunicação com o Let's Encrypt para obter o certificado SSL/TLS. O campo `Cache` é uma interface que define como e onde o Manager deve armazenar os certificados. Neste exemplo estamos usando `autocert.DirCache` que armazena os certificados em um diretório local. Esta é a forma mais fácil de começar, mas pode não ser a melhor alternativa quando o site é hospedado em múltiplos servidores, afinal cada servidor terá seu próprio cache.

O último passo é criar um `http.Server` que escuta a porta `443` e usa nosso `mux`. Criamos então o objeto `tls.Config` e atribuímos ao Server. Agora é que a **mágica** acontece. `GetCertificate` é a função que o servidor usa para carregar o certificado quando uma nova requisição HTTPS chega ao servidor. Este método nos da a oportunidade de escolher qual certificado usar ao invés de retornar sempre o mesmo. O que fazemos agora é delegar esta responsabilidade ao `certManager.GetCertificate` na qual primeiro procurará o certificado no cache, caso não seja encontrado, um novo certificado é emitido no Let's Encrypt através do protocolo ACME.

No começo de 2018, [Let's Encrypt desabilitou o desafio de TLS-SNI](https://community.letsencrypt.org/t/2018-01-11-update-regarding-acme-tls-sni-and-shared-hosting-infrastructure/50188). A recomendação agora é usar o [desafio HTTP](https://tools.ietf.org/html/draft-ietf-acme-acme-07#section-8.3), e é por isto que usamos o `certManager.HTTPHandler(nil)`.

Depois disto, basta iniciar nosso servidor com `server.ListenAndServeTLS("", "")`. Se você já usou HTTPS em Go antes, talvez se lembre de que estes dois parâmetros são o Certificado e a Chave Privada, mas quando estamos usando `autocert` isto não é necessário.

Vale a pena comentar que o `certManager.HTTPHandler(nil)` redireciona todo o tráfego HTTP para HTTPS automaticamente, mas isto pode ser customizado ao substituir o parâmetro nil por um HTTP Handler.

## Está na hora de testarmos!

*Você pode executar este código como qualquer outra aplicação Go, mas falhará se fizer isto em sua máquina local. O motivo é que o Let's Encrypt precisa que seu site esteja público através de um node de DNS conhecido. Quando executado localmente, o Let’s Encrypt não tem como encontrar seu site na internet e o processo de verificação de domínio falha.*

1. Crie um novo registro de DNS A apontando para o IP público de sua máquina virtual.
2. Compile seu código Go com `CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -o autossl`. Ajuste os parâmetros caso seu servidor não seja linux/amd64.
3. Copie o binário autossl para seu servidor.
4. Conecte-se ao servidor via SSH e execute o binário para iniciar o servidor.
5. Abra o navegador com o endereço de seu domínio configurado na etapa 1.

![](/public/images/2017/11/auto-ssl-golang.png)

Ta-Da! 🎉 

Você deverá ver a mensagem `Hello Secure World` e o símbolo verde de HTTPS.

Talvez demore alguns segundos para carregar pela primeira vez, isto acontece devido à uma série de troca de mensagens com o Let's Encrypt para geração do certificado. Porém as request subsequentes serão muito rápidas, afinal o certificado já está armazenado no cache.

## Sugestões e Notas Importantes:

1. Existem limites de quantos certificados podem ser gerados para o mesmo domínio. No atual momento de escrita deste post, o limite é de 20 certificados por semana. Pode parecer bastante, mas se o seu cache não estiver funcionando corretamente, este limite é facilmente comprometido. Para maiores informações, consulte a documentação oficial [https://letsencrypt.org/docs/rate-limits/](https://letsencrypt.org/docs/rate-limits/).
2. É sua responsabilidade decidir como e onde armazenar os certificados. O armazenamente local funciona bem para um único servidor, mas quando há um conjunto deles, este método é ineficiente. Lembre-se do limite acima!
3. Todos os certificados gerados pelo Let's Encrypt são válidos por 90 dias. Mas fique tranquilo, pois o **autocert** faz o gerenciamente dos certificados e os renova quando estiver próximo de expirar. Mas é sempre bom ficar de olho caso este processo falhe.
4. O limite de 20 certificados é por domínio, excluíndo sub-domínio. Então sub1.domain.com e sub2.domain.com compartilham do mesmo limite.
5. Como você não pode executar o autocert localmente, você terá que construir sua aplicação de forma que você possa escolher entre HTTP ou HTTPS, para que assim possa ser utilizado o HTTP durante o processo de desenvolvimento.

É isto ai, espero que tenha ajudado.

Um abraço!