---
layout: post
title: Um simples cache no lado servidor para Express.js em Node.js
lang: pt
tags: [express, nodejs, cache, performance]
ref: simple-server-side-cache-for-expressjs
---

Express é provavelmente o framework web mais extensível que vi até agora. Sua arquitetura baseada em middlewares torna muito fácil a adição de novas funcionalidades de forma padronizada e com mínimo esforço.

Neste post mostro como fazer um pequeno e simples, porém poderoso e útil, middleware para ajudar a aumentar o desempenho de suas aplicaçoes web com Express.js sem qualquer dependencia externa.

<!--more-->

## Sobre cache no lado servidor

`Caching` é provavelmnete a técnica mais comum para aumento de desempenho am qualquer aplicação, seja ela desktop, mobile ou web. Quando lidamos com aplicações web é recomendável fazermos bom uso dos cabeçalhos de resposta HTTP para que o lado cliente (browsers) possam *cachear* corretamente o conteúdo que você está servindo. Mas quando temos uma página complexa e que leva, por exemplo, 2 segundos para gerar a resposta em HTML, mesmo que habilitemos o cache do lado cliente, nosso servidor ainda assim terá que processar a requisição para cada diferente usuário acessando a aplicação.

Imagine a página inicial de um grande portal de notícias. Eles precisam processar o HTML toda vez que recebe um novo visitante?

É neste momento que o cache no lado servidor entra em ação. O objetivo do cache de servidor é responder o mesmo conteúdo (seja ele HTML, JSON, XML, etc.) independente de que usuário/visitante está solicitando a página. No exemplo do nosso site de notícias, o primeiro visitante terá que esperar os 2 segundos para o servidor processar o HTML da página inicial, armazenar no cache e então retornar o conteúdo para o cliente. Mas a partir do segundo visitante o conteúdo já estará armazenado em cache, sendo possível retornar o HTML em poucos milisegundos.

Existem várias maneiras de fazer isto, poderia ser feito pelo NGINX, ou então um CDN como CloudFlare, mas neste exemplo faremos uso de um middleware bem simples escrito em JavaScript e que pode ser incluído em qualquer applicação Express.js.

## Show me the code!

Faremos uso do pacote [memory-cache](https://www.npmjs.com/package/memory-cache) disponível no NPM. Este pacote apenas fornece métodos para adicionar, ler e remover objetos da memória.

Nosso middleware fica assim:

~~~javascript
var mcache = require('memory-cache');

var cache = (duration) => {
  return (req, res, next) => {
    let key = '__express__' + req.originalUrl || req.url
    let cachedBody = mcache.get(key)
    if (cachedBody) {
      res.send(cachedBody)
      return
    } else {
      res.sendResponse = res.send
      res.send = (body) => {
        mcache.put(key, body, duration * 1000);
        res.sendResponse(body)
      }
      next()
    }
  }
}
~~~

Ele basicamente verifica se a página já está *cacheada* utilizando sua URL como a chave. Se entrou, ótimo, obtenha o conteúdo e retorne. Se não encontrou, fazemos um *wrap* da função `send` para que possamos fazer o cache antes de efetivamente enviar a resposta para o cliente e então deixamos o Express.js seguir com o fluxo normal de processamento dos outros middlewares utilizando a funcão `next`.

E aqui está um exemplo de como fazer uso do middleware.

~~~javascript
app.get('/', cache(10), (req, res) => {
  setTimeout(() => {
    res.render('index', { title: 'Hey', message: 'Hello there', date: new Date()})
  }, 5000) //setTimeout was used to simulate a slow processing request
})
~~~

![](/public/images/server-side-cache-express.png)

Note que na rota acima foram definidos dois middlewares. O primeiro á uma chamada para nosso middleware de cache enquanto o segundo é realmente a função que irá processar a requisição. Neste caso, quando o servidor receber a primeira requisição para esta reta, ele irá interromper o processo por 5 segundos antes de enviar a resposta para o cliente. Após isto, as requisições subsequetnes irão retornar o conteúdo *cacheado* pelos próximos 10 segundos e por isto não irão aguardar os 5 segundos. O lado negativo deste modelo é quando você possui um conteúdo dinâmico. Neste exemplo passamos a data atual como um dos parâmetros para a view, e como esta data é utilizada para formar o HTML final, as requisições subsequentes apresentarão a mesma data até que o cache expire (10 segundos neste caso).

A parte legal deste modelo é que funciona com qualquer ContentType, seja ele HTML, JSON, XML, etc. Ou seja, pode ser utilizado tanto em aplicações web, REST-based API ou SOAP WebServices.

Você pode simplesmente adicionar este middleware em qualquer rota de suas aplicações Express.js que você desejar fazer cache.

*Importante:* Operações `PUT`, `DELETE` and `POST` nunca devem ser *cacheadas*.

Para este exemplo utilizamos um módulo do NPM para fazer o cache em memória, porém esta abordagem possui boas e más implicações.

- Cache em memória é a opção mais rápida possível;
- É fácil de programar, pois não há nenhuma dependencia externa;
- Nós perdemos todo o cache se o servidor ou o processo for finalizado;
- Já que o cache é armazenado na memória do próprio processo, ele não será compartilhado com múltiplos processos/servidores Node.js;

Outra opção muito comum é o uso de um serviço de cache distribuído como o [Redis](http://redis.io/). Se este for seu objetivo, já existe até um módulo NPM pronto para isto [express-redis-cache](https://www.npmjs.com/package/express-redis-cache).

## Código completo

~~~javascript
'use strict'

var express = require('express');
var app = express();
var mcache = require('memory-cache');

app.set('view engine', 'jade');

var cache = (duration) => {
  return (req, res, next) => {
    let key = '__express__' + req.originalUrl || req.url
    let cachedBody = mcache.get(key)
    if (cachedBody) {
      res.send(cachedBody)
      return
    } else {
      res.sendResponse = res.send
      res.send = (body) => {
        mcache.put(key, body, duration * 1000);
        res.sendResponse(body)
      }
      next()
    }
  }
}

app.get('/', cache(10), (req, res) => {
  setTimeout(() => {
    res.render('index', { title: 'Hey', message: 'Hello there', date: new Date()})
  }, 5000) //setTimeout was used to simulate a slow processing request
})

app.get('/user/:id', cache(10), (req, res) => {
  setTimeout(() => {
    if (req.params.id == 1) {
      res.json({ id: 1, name: "John"})
    } else if (req.params.id == 2) {
      res.json({ id: 2, name: "Bob"})
    } else if (req.params.id == 3) {
      res.json({ id: 3, name: "Stuart"})
    }
  }, 3000) //setTimeout was used to simulate a slow processing request
})

app.use((req, res) => {
  res.status(404).send('') //not found
})

app.listen(3000, function () {
  console.log('Example app listening on port 3000!')
})
~~~

É isto ai.

Abraços.