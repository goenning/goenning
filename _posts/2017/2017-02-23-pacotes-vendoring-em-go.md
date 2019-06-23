---
layout: post
title: "Pacotes e vendoring em Go"
lang: pt
tags: [go, web]
series: primeirawebappgo
ref: packages-vendoring-in-go
---

No último post desta série terminamos uma aplicação web simples, porém completamente funcional usando apenas a biblioteca padrão. Durante este post você aprenderá como funciona o gerenciamento de pacotes em Go usando um pacote de terceiros que é bem famoso.

![](/public/images/2017/02/go-packages.jpg)

### Pacotes

Projetos Go são organizados em pacotes. Você pode criar seus próprios pacotes, como o `main` que foi criado no post anterior, ou você também pode fazer uso de pacotes de terceiros para acelerar o proceso de desenvolvimento. Por padrão o Go vem com o que é conhecido como biblioteca padrão (standard library ou stdlib). Nós utilizamos isto antes quando escrevemos o import de `html/template` and `net/http`.

Como Go é uma linguagem compilada, quando você compila um projeto que referencia um ou mais pacotes, seu código e todo o código fonte destes pacotes são carregados de seus diretórios locais e compilados em um único executável contendo linguágem de máquina. Isto sginifica que nós não precisamos de nenhuma ferramento do Go instalada no ambiente de produção, tudo está contido neste único exectuável, até mesmos os pacotes externos.

Vamos começar a fazer uso de pacotes para organizar nosso projeto.

Go não permite que nós tenhamos dois pacotes dentro de um mesmo diretório, por isto precisamos criar um diretório para nosso novo pacote e colocar nosso código la. Crie um diretório chamado `env` e um arquivo `env.go` dentro dele.

```go
package env

import "os"

//GetEnvOrDefault returns environment value if it's available, otherwise returns given default value
func GetEnvOrDefault(env string, def string) string {
	v := os.Getenv(env)
	if v != "" {
		return v
	}
	return def
}
```

Diferente de outras linguagens onde você deve explicitamente referenciar quais objetos/classes que você deseja importar, Go só precisa saber o nome do pacote, então você pode nomear seu arquivo da forma que você achar melhor, assim que um pacote for importado, você terá acesso à todos os objetos exportados por ele.

Objetos exportados? Eu ainda não falei sobre isto, correto?

Este á mais um dos recursos legais do Go. Você não precisa explicitamente export nada or definir seus objetos como privado ou público, tudo o que você precisa fazer é seguir uma convenção.

Tudo o que começa com letra maíuscula é exportado do pacote. Quando começar com letra minúscula, o objeto é então considerado interno ao pacote e não pode ser usado fora dele. Em nosso exemplo, `GetEnvOrDefault` é público, pois precisamos usá-lo no pacote `main`. Legal, não é?

Agora podemos alterar nosso ponto de entrada para usar nosso novo pacote e sua função.

```go
package main

import (
	"fmt"
	"html/template"
	"net/http"

	"github.com/goenning/gostore/env"
)

func index(w http.ResponseWriter, r *http.Request) {
	tpl, _ := template.ParseFiles("index.html")
	data := map[string]string{
		"Title": "Go Store :)",
	}
	w.WriteHeader(http.StatusOK)
	tpl.Execute(w, data)
}

func main() {
	port := env.GetEnvOrDefault("PORT", "8080")
	http.HandleFunc("/", index)
	fmt.Printf("Server is up and listening on port %s.\n", port)
	http.ListenAndServe(":"+port, nil)
}
```

Note como precisamos adicionar o caminho completo de nosso pacote `env`. Está curioso para saber o motivo? Apenas continue lendo :)

### Go e seu Gerenciador de Dependências

As dependências em Go são gerenciadas de uma maneira totalmente diferente quando comparadas com plataformas como .NET, Java e Node.js. Ao invés de um registro de dependências, Go utiliza endereços públicos que hospedam arquivos Go utilizando um sistema de VCS como o Git, SVN ou Mercurial.

Todos os pacotes são importados através de seu caminho completo começando de `$GOPATH/src`, o que explica a necessidade de definir o `$GOPATH` durante a instalação do Go. A única exceção para esta regra é a `stdib` que é importada de `$GOROOT/src`.

Vamos começar fazendo algo bem simples. Como vimos antes, o router padrão do Go não é muito flexível e possui alguns efeitos colaterais. Vamos substituí-lo pelo `github.com/gorilla/mux`, um router escrito em Go bastante popular e bem robusto.

Você pode obtê-lo usando o seguinte comando em seu terminal.

> go get github.com/gorilla/mux

O que `go get` fará é baixar o código fonte do GitHub e colocar os arquivos em `$GOPATH/src/github.com/gorilla/mux`. Você pode confirmar isto inspecionando o diretório `$GOPATH` em seu sistema operacional.

Ótimo! Agora você pode criar qualquer aplicação em Go e utilizar `github.com/gorilla/mux`.

O seguinte código é um exemplo de como trocar nosso servidor HTTP para usar o router do `github.com/gorilla/mux` ao invés do padrão.

```go
package main

import (
	"fmt"
	"html/template"
	"net/http"

	"github.com/goenning/gostore/env"
	"github.com/gorilla/mux"
)

func index(w http.ResponseWriter, r *http.Request) {
	tpl, _ := template.ParseFiles("index.html")
	data := map[string]string{
		"Title": "Go Store :)",
	}
	w.WriteHeader(http.StatusOK)
	tpl.Execute(w, data)
}

func main() {
	port := env.GetEnvOrDefault("PORT", "8080")
	r := mux.NewRouter()
	r.HandleFunc("/", index)
	fmt.Printf("Server is up and listening on port %s.\n", port)
	http.ListenAndServe(":"+port, r)
}
```

A única coisa que mudou é que agora criamos um novo router usando `mux.NewRouter()`, registramos todos os handlers nele e o utilizamos para servir uma determinada porta. Lembra que antes nós tínhamos o valor `nil` como o segundo parâmetro? Este parâmetro é o router, então quando for `nil`, o router padrão é utilizado.

Você pode compilar e executar seu prjeto Go exatamente como antes.

Parabéns! Você acaba de aprender a usar um pacote de terceiros em Go :)

### Vendoring 

Este projeto funcionará em seu computador desde que você tenha executado o comando `go get` para baixar a dependência. Mas como podemos compartilhar este código entre nosso coletas ou a comunidade e garantir que todos tenham as dependências baixas e, o mais importante, a versão correta de cada dependência.

É aí que o conceito de `vendoring` entra em jogo. Basicamente á uma funcionalidade introduzida no Go 1.5 que permite aplicações Go utilizar dependências não só de `$GOPATH/src`, mas também de um diretório chamado `vendor` dentro de cada projeto.

Isto significa que você pode colocar suas dependências dentro do diretório vendor ao invés de compartilhá-la globalmente no diretório $GOPATH. O compilador do Go primeiramente procurará pelos pacotes dentro do diretório vendor, antes de procurar em $GOPATH.

Foi ai que surgiu um punhado de ferramentas criadas pela comunidade que permite que as aplicações descrevam explicitamente suas dependências em um arquivo e a opção de restaurá-las através de um simples comando.

Glide, govendor e godep são alguns das ferramentas mais populares, podendo ser facilmente comparada com NPM, Maven e NuGet, por exemplo.

Você pode atingir o mesmo resultado com qualquer ferramenta, mas neste tutorial vamos utilizar o Glide, pois acredito ser o mais simples e talvez o mais usado.

### Usando o Glide

O Glide está disponível em diversas plataformas, faça o download e instale a versão correspondente ao seu sistema operacional em [https://github.com/Masterminds/glide/releases](https://github.com/Masterminds/glide/releases).

Feito isto, você pode verificar se tudo ocorreu bem executando o comando `glide -v`.

O primeiro comando que deve ser utilizado uma única vez para cada novo projeto é `glide init`, que basicamente criará um arquivo `glide.yaml` que descreve todas as dependências de nosso projeto.

Como nós já havíamos importado `github.com/gorilla/mux` usando `go get`, este comando o descobrirá e automaticamente definí-lo como uma dependência do projeto. Mas você poderia instalar este ou qualquer outra dependência executando o mesmo comando `go get`, apenas trocando `go` por `glide`.

> glide get github.com/gorilla/mux

Quando este comando é executado, duas coisas acontecem.

1. `github.com/gorilla/mux` é baixado e colocado em seu diretório vendor ao invés de `$GOPATH/src`.
2. `glide.yaml` é atualizado com sua nova dependência.

```yaml
package: github.com/goenning/gostore
import:
- package: github.com/gorilla/mux
  version: ^1.3.0
```

Agora você tem um arquivo `glide.yaml` que descreve todas as dependências de seu projeto, então qualquer pessoa que tenha Glide instalado pode simplesmente executar `glide install`, compilar e executar o projeto.

Também há o arquivo `glide.lock`, que é basicamente um arquivo de lock usado pelo Glide para garantir que seu projeto estará usando sempre a mesma versão de cada dependência quando `glide install` é executado. Para uma referência completa do Glide, verifi

### Auto compilação de nossa aplicação usando Gin CLI

`Gin` é uma ferramenta de linha de comando (CLI) escrita em Go que executa nossa aplicação web e faz com que ela seja reiniciada sempre que um arquivo Go for alterado.

Podemos isntalar esta ferramente através do `go get`.

> go get https://github.com/codegangsta/gin

Como Gin é um pacote CLI, ele é então compilado em um binário e colocado no diretório  `$GOPATH/bin`, por isto é recomendado que sempre adicione este diretório em sua variável de ambiente `$PATH` para que seja possível executar estes comandos de qualquer lugar.

Gin (e muitos web hosts, como Heroku) disponibilizam uma variável de ambiente nomeada `$PORT` que sua aplicação deve escutar para que funciona corretamente, ou seja, você não pode colocar sua aplicação para escutar uma porta aleatória. E é exatamente assim que o Gin também funciona, então tenha certeza que você não pulou a primeira parte deste post onde fizemos o uso da função `GetEnvOrDefault`.

Ao invés de `go run`, você agora pode executar apeans `gin` e navegar até o endereço `http://localhost:3000` para ver sua aplicação funcionando.

Assim que você fizer qualquer alteração em um arquivo Go, Gin recompilará e reexecutará seu app sem que você tenha que executar nada no terminal, apenas recarregue a página e veja a diferença. Esta ferramente é ótimo para acelerar o desenvolvimento, use e abuse :)

### Vamos recapituar o que aprendemos até agora:

1. Como organizar código em Go usando pacotes
2. Como gerenciar as dependências de seu projeto.
3. Podemos instalar ferramentas de linha de comando escritas em Go utilizando um único comando

Isto é tudo por hoje pessoal, espero vocês no próximo post.

O código fonte deste projeto ainda está disponível em [https://github.com/goenning/gostore](https://github.com/goenning/gostore).

Em caso de dúvidas ou sugestões, deixe seu comentário logo abaixo. 

Um abraço e até mais!
￼
{% include _series_primeirawebappgo.html %}