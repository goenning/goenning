---
layout: post
title: "Cache no Servidor com Go"
lang: pt
tags: [go, web, performance, cache]
ref: server-side-cache-go
description:
  Go é muito rápido e todos nós sabemos disto. Mas como fazemos para nossas aplicações web serem ainda mais rápidas? Neste post vou mostrar como podemos obter tempos de resposta ainda menores using cache no lado do servidor, afinal, performance é importante e ninguém gosta de ficar olhando barra de progresso, não é?
---

![](/public/images/2017/03/server-side-cache-go.png)

Go é muito rápido e todos nós sabemos disto. Mas como fazemos para nossas aplicações web serem ainda mais rápidas?

Quando falamos em performance, as discussões geralmente acabam em algum tipo de cache. Neste post vou mostrar como podemos obter tempos de resposta ainda menores using cache no lado do servidor, afinal, performance é importante e ninguém gosta de ficar olhando barra de progresso, não é?

## O que é cache no lado do servidor?

A maioria dos desenvolvedores web já está familizariado com o cache de navegador, aquele que usa o cabeçalho HTTP `Cache-Control`. Este parâmetro que é definido pelo servidor é usado para informar ao navegador quando e por quanto tempo é que ele pode fazer o cache da resposta HTTP. Isto é extremamente importante a devemos sempre usar para arquivos estáticos como JavaScript, CSS e Imagens.

Mas e as páginas HTML? Podemos fazer cache delas também? Certamente não é recomendável fazer isto no lado do navegador, afinal se o conteúdo de nossa página for alterado, alguns visitantes continuariam a visualizar o conteúdo antigo. Imagine um portal de notícias como o G1 ou R7. Como eles podem oferecer um serviço tão rápido para milhões de pessoas? Se um novo artigo for publicado, todos os visitantes devem podem visualizar imediatamente na próxima atualização da página.

É ai que entra o cache no lado do servidor. Montar a página inicial de um portal de notícias é uma operação que exige de bastante IO como chamadas ao banco de dados, leitura de arquivos ou chamadas à API externas. Após o HTML da página inicial é montada para o primeiro visitante, é possível armazenar isto em cache e utilizar este mesmo HTML para todas as requisições subsequentes à mesma página. Ao fazermos isto no servidor, temos total controle para invalidar o cache a qualquer momento.

Isto não remove a necessidade de fazer uma requisição HTTP para a página HTML, mas certamente agiliza a forma como que o servidor responde.

### Como fazemos isto em Go?

Go não é apenas rápido, é também muito fácil, então uma implementação simples requer apenas algunas linhas de código.

A parte mais difícil é decidir onde é que você vai armazenar o cache. Os meios mais comuns são: em memória, disco ou banco de dados. Qualquer um destes vai funcionar muito bem, mas entender os pontos fortes e negativos de cada um é muito importante antes de tomar uma decisão.

**Em memória**: Toda página é armazenada na memória do process que está hospedando a página. Isto faz com que o acesso seja o mais rápido possível e também o mais fácil de implementar. O grande ponto negativo é que como o cache é local, cada servidor terá sua própria cópia. Além disto, caso o processo reinicie por qualquer motivo, o cache é perdido e terá será reconstruido assim que receber novas requisições.

**Disco**: Toda página é armazenada no disco, seja ele local ou remoto. Caso o disco seja remoto, certamente esta não é a opção mais rápida já que haverá um tráfego de rede a cada operação de escrita e leitura. A grande vantagem é que é barato e resiliente, já que o cache é mantido quando a aplicação é reiniciada.

**Banco de Dados**: As páginas são armazenadas em um banco de dados, pode ser SQL ou chave-valor (Redis, Memcached). O fato é que Redis é uma das opção mais usadas para nesta situação. Não chega a ser tão rápido quanto o armazenamento em memória, já que é necessário algumas operações na rede, mas o cache é compartilhado por todos os servidores de aplicação, ou seja, o é resiliente e centralizado.

### Eu quero ver código!

O código fonte complete desta aplicação de demonstração está disponível no [GitHub](https://github.com/goenning/go-cache-demo). A aplicação está em execução no [Heroku](https://go-cache-demo.herokuapp.com/) também. Vou ressaltar aqui as partes importantes do código.

O primeiro passo foi criar uma interface chamada de [Storage](https://github.com/goenning/go-cache-demo/blob/master/cache/cache.go) que pode ser usada para ler ou escrever no cache. Isto é uma interface, pois o consumidor não deveria estar acoplado à uma implementação específica.

```go
type Storage interface {
	Get(key string) []byte
	Set(key string, content []byte, duration time.Duration)
}
```

Em seguida temos duas structs que implementam esta interface, [memory.Storage](https://github.com/goenning/go-cache-demo/blob/master/cache/memory/cache.go) que utiliza um objeto `map` para armazenar o cache e o [redis.Storage](https://github.com/goenning/go-cache-demo/blob/master/cache/redis/cache.go) que utiliza um cliente de Redis.

A implementação destas structs é simples de entender, então vou pular esta parte. Você pode tentar implementar um Storage para gravar o cache em disco. Apenas crie uma nova struct que implemente a interface e utilize o pacote `os` ou `ioutil` do Go para ler e escrever em arquivos.

`cached` é um middleware HTTP que é executado antes do HTTP handler e retorna a página imediatamente caso ela já esteja no cache. Caso contrário, o handler é executado e a responsta é armazenada no cache por algum tempo. Pelo fato desta funcão ser um middleware, fica fácil habilitar/desabilitar o cache para página específicas. Continue lendo para ver um exemplo.

Estou usando `RequestURI` como a chave do cache, pois o objetivo aqui é fazer o cache utilizando a URL completa, incluindo o path e a querystring. Isto significa que as páginas `/users?page=1` e `/users?page=2` são armazenadas separadamente, mesmo que o HTTP handler seja o mesmo para ambas URL.

O código do middleware é o seguinte.

```go
package main

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"time"
)

func cached(duration string, handler func(w http.ResponseWriter, r *http.Request)) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {

		content := storage.Get(r.RequestURI)
		if content != nil {
			fmt.Print("Cache Hit!\n")
			w.Write(content)
		} else {
			c := httptest.NewRecorder()
			handler(c, r)

			for k, v := range c.HeaderMap {
				w.Header()[k] = v
			}

			w.WriteHeader(c.Code)
			content := c.Body.Bytes()

			if d, err := time.ParseDuration(duration); err == nil {
				fmt.Printf("New page cached: %s for %s\n", r.RequestURI, duration)
				storage.Set(r.RequestURI, content, d)
			} else {
				fmt.Printf("Page not cached. err: %s\n", err)
			}

			w.Write(content)
		}

	})
}
```

Para usá-lo é necessário encapsular a chamada ao HTTP handler dentro da chamada à funcão `cached`. Graças ao pacote `time` do Go, podemos usar uma string fácil de ler para representar a duração total do cache. Por exemplo, `10s` é muito mais fácil de entender do que `10 * 1000`. No exemplo a seguir, apenas o HTTP handler index é que está sendo cacheado.

```go
// tanto index qaunto about são: func (w http.ResponseWriter, r *http.Request) { ... }
http.Handle("/", cached("10s", index)) 
http.HandleFunc("/about", about)
http.ListenAndServe(...)
```

Veja se seguintes images de duas requisições subsequentes ao mesmo endereço.

A primeira requisição demora **2 segundos** enquanto a segunda apenas **27ms**. A resposta possui o **mesmo conteúdo e tamanho**.

![](/public/images/2017/03/load-one.png)

![](/public/images/2017/03/load-two.png)

10 segundos após a primeira requisição, o cache é removido e a próxima requisição irá novamente demorar 2 segundos.

### O que você deve tomar cuidado quando estiver implementando cache no lado do servidor

O primeiro passo é **nuncan** fazer cache de requisições POST, PUT ou DELETE, afinal estas operação são usadas para alterar dados e não retornar, por isto não faz sentido fazer cache disto. Dito isto, apenas requisições GET devem ser cacheadas. **Dica**: é possível implementar algumas validações no middleware para evitar enganos como este :)

Tome um cuidado extra com conteúdo baseado no usuário autenticado. Aplicações e sites que possuiem opção de autenticação também podem ser cacheadas, porém um cuidado extra deve ser tomado para que a chave de identificação do usuário também deve fazer parte da chave do cache, caso contrário sua aplicação irá retornar conteúdo de um usuário para outro, o que seria um grande vazamento de dados. A desvantagem aqui é a quantidade de páginas a ser cacheadas será bem maior, tenha isto em mente!

Outra sugestão é implementar alguma forma que seja fácil de desativar o cache, pois isto pode ser bem incoveniente durante o desenvolvimento.

### Parabéns 🎉 

Você acaba de aprender como fazer da web um lugar melhor para todos nós. Agora você já sabe como fazer algo rápido ficar ainda mais rápido! 

Um abraço!