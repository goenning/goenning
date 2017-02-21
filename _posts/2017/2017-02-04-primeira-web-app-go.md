---
layout: post
title: Sua primeira aplicação web usando Go
comments: true
lang: pt
tags: [go, golang, web, gostore]
series: primeirawebappgo
ref: your-very-first-web-app-go
---

Estou começando hoje uma nova série no blog de como construir sua primeira aplicação web usando Go. Farei isto seguindo um modelo de "baby steps", então você pode esperar por uma série longa cobrindo tudo que você precisa (ou deveria) fazer/usar para construir uma aplicação web, como Go tools, GOPATH, testes automatizados, vendoring, banco de dados e, é claro, com hospedagem na nuvem.

Você está pronto?

### Ok. Vamos lá!

A aplicação que iremos construir **juntos** é chamada de **Go Store**.

O objetivo por trás deste app é permitir que qualquer desenvolvedor tenha sua própria loja na web. Não teremos carrinho de compras, checkout ou qualquer processo de pagamento, será apenas uma página para mostrar tudo o que está disponível para venda e a forma de contato.

[Recentemente saí do Brasil](/2016/12/03/de-mudanca-para-irlanda/) e por isto tive que fazer um site estático para anunciar tudo o que eu tinha para vender antes de me mudar definitivamnete.

Se tivesse feito este app antes, poderia tê-lo usado para publicar minha própria loja dinâmica, sem ter que ficar atualizando um arquivo HTML sempre que atualizava um produto.

O código fonte deste projeto estará sempre disponível em [https://github.com/goenning/gostore](https://github.com/goenning/gostore).

### Instalação

A primeira coisa que você terá que fazer é, obviamente, instalar o Go. Se você ainda não fez isto, siga as instruções da [documentação oficial](http://www.golangbr.org/doc/instalacao). Tenha certeza que você seguiu todos os passos, pois a instalação do Go não é apenas next, next, finish, você terá que configurar algumas variáveis de ambiente.

### Sua primeira aplicação usando Go

O primeiro passo é configurar nossa estrutura do projeto. 

Na minha máquina estou usando o o caminho `$GOPATH/src/github.com/goenning/gostore` como raiz do meu projeto. Fique a vontade para usar o que você quiser, desde que esteja dentro de `$GOPATH/src/`.

Feito isto, crie um arquivo `main.go` e cole o seguinte código. Isto é tudo que você precisa para criar uma aplicação web bem simples em Go.

```go
package main

import (
	"fmt"
	"html/template"
	"net/http"
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
	http.HandleFunc("/", index)
	fmt.Println("Server is up and listening on port 8080.")
	http.ListenAndServe(":8080", nil)
}

````

‘main‘ é o ponto de entrada de nosso programa, então este será a primeira função a ser chamada quando nosso programa for executado.

`http` é um pacote da biblioteca padrão do Go usada para criar servidores e clientes HTTP. Neste exemplo estamos criando um servidor HTTP quando chamamos a função `ListenAndServe` passando uma porta a ser escutada.

Mas antes de criarmos o servidor, precisamos configurar todos as rotas que gostariamos de que nossa aplicação seja capaz de processar.

Para fazer isto chamamos a função `http.HandleFunc` passando dois argumentos. O primeiro argumento é o caminho da URL, já o segundo é a função que será chamada para processar a requisição. Esta função é comumente chamada de `HTTP Handler`.

Nosso `HTTP handler` é muito simples. Primeiro carregamos o template chamado `index.html`. Em seguida fazemos a renderização deste template e uma estrutura de dados qualquer e escrevemos o resultado em nossa variável w, que é um ResponseWriter. Além disto também atribuimos o status code 200. Ambos o conteúdo e o status code serão então enviados de volta para o cliente, fazendo com que o browser mostre o conteúdo HTML de nosso template.

Por questões de simplicidade, ignorei completamente qualquer erro que possa ocorrer em `ParseFiles` and `Execute`. Isto não é uma boa prática, pois devemos sempre tratar os errors corretamente. Deixei isto para mais tarde quando faremos algumas refatorações neste código.

Abaixo está o conteúdo de nosso template `index.html`.

{% raw %}
```html
<!doctype html>
<html>
    <head>
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width,initial-scale=1">
        <title>{{.Title}}</title>
    </head>
    <body>
      Welcome to your own <strong>{{.Title}}</strong>
    </body>
</html>
```
{% endraw %}

Veja que temos duas variáveis dentro de chaves duplas. Estas variáveis são substituidas pelo conteúdo da structura que você passa no segundo parâmetro da função `tpl.Execute`.

Você pode testar esta aplicação executando `go run main.go` em seu terminal favorito e navegando até o endereço `http://localhost:8080/` em qualquer navegador.

Se tudo ocorreu bem você deverá ver uma mensagem de boas vindas impressa na tela.

Sim, isto é tudo que você precisa. Nada de Apache, Tomcat ou ISS. Por estarmos utilizando o próprio servidor HTTP do Go não é necessário nenhum outro container web.

A parte interessante é que, mesmo tendo configurado nosso servidor para processar apenas a rota `/`, você pode navegar para **qualquer** página neste endereço e o retorno será sempre este mesma mensagem.

Isto pode parecer estranho para alguns, mas é assim que o router padrão do Go funciona. Você consegue mais informações sobre isto na [documentação oficial](https://golang.org/src/net/http/server.go?s=57308:57433#L1890) deste pacote. 

Se você não gosta deste comportamento é possível facilmente escrever seu próprio router or utilizar qualquer alternativa open source, existem dezenas de opções. Faremos uso de uma delas no futuro próximo.

### Vamos recapituar o que aprendemos até agora:

1. `http` faz parte da biblioteca padrão do Go e pode ser usada para criar servidores e clientes HTTP.
2. Não precisamos de nenhum container web para hospedar nossa aplicação escrita em Go.
3. O router padrão do Go é fácil de usar, mas possui alguns efeitos colaterais.

Isto é tudo por hoje pessoal. Espero vocês no próximo post quando introduziremos vendoring, router customizado, mocked data e templates.

Em caso de dúvidas ou sugestões, deixe seu comentário logo abaixo. 

Um abraço e até mais!
￼
{% include _series_primeirawebappgo.html %}
