---
layout: post
title: Adicionando dados customizados em binários Go durante a compilação
comments: true
lang: pt
tags: [go, goland, ldflags]
ref: adding-custom-data-go-binaries-compile-time
abstract: As vezes é realmente útil obter informações sobre seus binários Go, por exemplo: quando ele foi compilado, qual o usuário que o compilou, qual o hash do último commit usado, etc. Estas informações são especialmente úteis para versionamento de binários, para análise de bugs ou então só para ter uma referência para o código fon
---

As vezes é realmente útil obter informações sobre seus binários Go, por exemplo: quando ele foi compilado, qual o usuário que o compilou, qual o hash do último commit usado, etc. Estas informações são especialmente úteis para versionamento de binários, para análise de bugs ou então só para ter uma referência para o código fonte.

Como você faria isto?

- Poderíamos criar um arquivo `json/yml/xml` para armazenar estas informações. Sim, poderíamos, mas você quer atualizar manualmente a cada nova compilação? Você também teria que distribuir dois arquivos ao invés de apenas um.
- Que tal uma estrutura global no pacote `version`? Bom, já seria melhor, pois no final teríamos apenas o binário e não mais um arquivo de metadatos. Mas ainda assim, teríamos que atualizar o conteúdo da estrutura manualmente a cada compilação.

### Parâmetros de compilação ao resgate

Já ouviu falar de `-ldflags`?

Este parâmetro de compilação é usado pelo `go install|build` para sobrescrever variáveis Go variables em nossos programas.

Então se tivermos isto em nosso `main package`.

```
package main

var saySomething string
```

Se executarmos `go build -ldflags "-X main.saySomething=HelloWorld"`, o compilador iria atribuir o valor `HelloWorld` para a variável `main.saySomething`.

Você também poderia ter o seguinte código.

```go
package main

import (
	"fmt"
)

var (
	buildTime  string
	commitHash string
)

func main() {
	fmt.Printf("Build Time: %s\n", buildTime)
	fmt.Printf("Commit Hash: %s\n", commitHash)
}
```

E compilar assim.

> go build -ldflags "-X main.buildTime=$(date +"%Y.%m.%d.%H%M%S") -X main.commitHash=$(git log --pretty=format:'%h' -n 1)"

O resultado seria algo assim.

```
Build Time: 2017.01.25.070111
Commit Hash: 1e4e7b5
```

Não há problema se você não atribuir um valor para estas variáveis durante a compilação, pois o máximo que acontece é elas ficaram sem valor. Ok?

### Trabalhando com pacotes

Eu poderia encerrar o post aqui, mas não posso deixar você sair sem saber disto.

Como você pode ver em nosso último exemplo, foi necessário especificar ambos o pacote (`main`) e o nome da variável  (`buildTime` e `commitHash`). Isto significa que também podemos utilizar este parâmetro para injetar variáveis em pacotes. 

Se tivéssemos o seguinte pacote.

```
package info

var (
	BuildTime string
	CommitHash string
)
```

Poderíamos então definir seus valores utilizando a seguinte instrução.

> go build -ldflags "-X github.com/goenning/hello-go/info.BuildTime=$(date +"%Y.%m.%d.%H%M%S") -X github.com/goenning/hello-go/info.CommitHash=$(git log --pretty=format:'%h' -n 1)"

Apenas lembre que como estamos escrevendo um pacote que será importado em algum outro lugar, nossas variáveis precisam começar com uma letra maíuscula. Outra opção seria deixá-las interna e implementar algum tipo de encapsulamento para ter acesso à estas valores de fora do pacote.

### Isto é tudo por hoje

Espero que estas dicas possam te ajudar a construir binários ricos em informações que te ajudem a evitar dores de cabeça durante a caça aos bugs :)
