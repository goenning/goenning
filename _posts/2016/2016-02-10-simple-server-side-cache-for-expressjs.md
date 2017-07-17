---
layout: post
title: Simple server side cache for Express.js with Node.js
comments: true
lang: en
tags: [express, nodejs, cache, performance]
ref: simple-server-side-cache-for-expressjs
---

Express is the most extensible web framework I've seen so far. The framework's middleware architecture makes it easy to plug-in extra features with minimal effort and in a standardized way.

For this article we'll cover a very small and simple, yet powerful and useful middleware that will help you boost your express web application performance without any external dependency.

## About server-side cache

`Caching` is a commonly used technique to improve the performance of any application, be it desktop, mobile or web. When dealing with web applications we can make good use of client side caching using response headers which all browsers currently support. But, what if we have a complex and heavy page that takes 2 second to generate the HTML output? Even if we enable client-side cache for this page, the web server will still need to render the page for each different user accessing our web application. Think about the home page of a large news portal, do they process their HTML over and over again for each visitor?

This is where server-side cache comes in handy. The goal of server-side cache is responding to the same content for the same request independently of the client's request. In our example above, the first request that reaches our server would still take 2 seconds to generate the HTML, but the following requests would hit the cache instead and the server would be able to send the response in a few milliseconds.

There are many ways of doing it, it could be done with NGINX or a CDN like CloudFlare, but in this example we'll see how to do it with Node.js and Express with minimal work and in a flexible way.

## Show me the code!

Our goal here is to enable server-side cache for our application with minimal effort. So, let's do it!

We'll make use of [memory-cache](https://www.npmjs.com/package/memory-cache) npm module in order to be able to add content to cache. Our cache middleware is the following.

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

It'll basically look for a cached value using request's url as the key. If it is found, it is sent directly as the response. If it's currently not cached, it'll wrap Express `send` function to cache the response before actually sending it to client and then calling the next middleware.

This is a very basic example on how to cache a heavy processing page.

~~~javascript
app.get('/', cache(10), (req, res) => {
  setTimeout(() => {
    res.render('index', { title: 'Hey', message: 'Hello there', date: new Date()})
  }, 5000) //setTimeout was used to simulate a slow processing request
})
~~~

![](/public/images/server-side-cache-express.png)

Note that the above route contains two middlewares. The first one is the cache reference and the second one is the real middleware that handles the request. In this case, when the server receives the first request for this route, it will wait for 5 seconds before sending to client. But after that, consecutive calls will get the cached response body for the next 10 seconds and, of course, it will not have to wait those 5 seconds anymore. The downside is if you have something that has to be dynamic. In the above route we have passed the current date as an argument to the view engine. The cached response body will have this same Date until the cache expires (10 seconds in this case).

The cool thing here is that it works for routes that responds with HTML, JSON, XML or any other content-type. It can be used to boost a simple web site up to some heavy, complex, REST-based express application.

You can easily plug it into any existing express web application by simple adding the `cache` middleware for each route you may want to cache.

*Important:* `PUT`, `DELETE` and `POST` methods should never be cached.

For this example we have used a npm module that caches the content in memory, this has some good and bad implications.

- In-memory cache is the fastest option available;
- It's easy to work with, no external dependency needed;
- We'll lose the cached content if the server or the process goes down;
- Since it stores cached content in it's own process memory, it will not be shared between multiple node.js process;

Another option to solve most of this issues is using a distributed cache service like [Redis](http://redis.io/). It could be done with a single npm module [express-redis-cache](https://www.npmjs.com/package/express-redis-cache) that already implements the Express a middleware.

## Full code

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

Happy coding!

Cheers!
