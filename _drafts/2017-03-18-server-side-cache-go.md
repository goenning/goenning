---
layout: post
title: "Server side cache with Go"
comments: true
lang: en
tags: [go, golang, web, performance, cache]
cover: /public/images/2017/03/server-side-cache-go.png
abstract: 
  Go is fast and everyone knows that. But how can we make it ever faster when running web applications on it? On this post I'll try to cover how can we achieve an even better response time for web applications using server side cache, because you know, performance IS a feature and I don't know anyone who enjoys spinning wheels.
---

![](/public/images/2017/03/server-side-cache-go.png)

Go is fast and everyone knows that. But how can we make it ever faster when running web applications on it?

When talking about speed, discussions usually end up with some sort of caching strategy. On this post I'll try to cover how can we achieve an even better response time for web applications using server side cache, because you know, performance **IS** a feature and I don't know anyone who enjoys spinning wheels. Do you?

### What is server side cache?

Most web developers are familiar with browser cache using HTTP header `Cache-Control`, which is basically how the server can instruct the browser on when and how long the browser can cache the resource. This is extremely useful and you should definitely use it for static assets like JavaScript, CSS and Images.

But what about HTML pages? How do we cache them? It's certainly not useful to cache it on the browser, because if your page changes, the user won't see the new content anytime soon. Let's take for instance a news portal like CNN, how do you serve the home page for millions of people so fast? If a new article is published, everyone needs to see it on the next refresh. 

That's where server cache comes into play. Building the index page of a news portal possible requires multiple IO operations like database queries or API calls. After the HTML of the index page is built for an user it's possible to cache it on the server and use this cached version to respond to all subsequent requests to the same page. By doing this on the server, we have full control on when to invalidate a given set of cached content when a new article is published. 

It does now save the user from sending a HTTP request like the browser cache does, but it'll certainly speed up the the server respond to it.

### How do we do it in Go?

Go is not just fast, it's also easy, and thus implementing this is not different.

The difficult part is to decide where you want to cache the page. Common strategies are usually to store it in process memory, disk or a database. Either of these approaches are fine, but understanding the drawbacks of each of them is important to make a decision.

**In-Memory**: Every page is cached on your web application's process memory, which makes it an excellent candidate for the fastest cache you'll ever have and the easiest to implement. The drawback is that if you have multiple servers (which you should probably have), you'll end with N copies of these cached content. If the process restarts for any reason, it'll lose all the cached content and thus slowing down the first request again.

**Disk**: Cached pages are stored on a disk. This is certainly not the fastest option available as the server needs to read from disk and maybe even do some network operations if it's not a local disk. The biggest advantage is that they are cheaper than memory and very resilient as they can survive reboots from application server.

**Database**: Cached pages are stored in a database, it could be SQL or a key-value storage, doesn't matter. The fact is that Redis is the king on this field. It's a high performance and battle tested in-memory database widely used. It's not as fast as process in-memory, because it requires network calls, but content is shared across all servers, so they are not duplicated and neither require resources from the application server.

### Talk is cheap, show me the code!

The full source code of a demo application is available at [GitHub](https://github.com/goenning/go-cache-demo) and running on [Heroku](https://go-cache-demo.herokuapp.com/). I'll highlight here the important bits of this project.

First thing I have created is an interface named [Storage](https://github.com/goenning/go-cache-demo/blob/master/cache/cache.go) that my application can use to get/set cached pages. It's an interface because the application doesn't care where it's going to be stored.

```go
type Storage interface {
	Get(key string) string
	Set(key, content string, duration time.Duration)
}
```

Then we have two structs that implement this interface, [memory.Storage](https://github.com/goenning/go-cache-demo/blob/master/cache/memory/cache.go) that uses a map object to store all the content and [redis.Storage](https://github.com/goenning/go-cache-demo/blob/master/cache/redis/cache.go) that uses a third-party Redis client.

The implementaton of these structs are pretty straightforward, so I'll skip and go to the important part. If you want to give the disk strategy a try, just create a new struct that implements the interface just like the others.

`cached` is an http middleware that runs before the http handler and returns the content straight away if the page is already cached. If it's not, the handler is executed and its body is cached for a given period of time. Because it's a middleware, it's really easy to enable and disable it for certain routes. Keep reading for a concrete example.

I'm using `RequestURI` as the key for my storage because I want to cache the pages based on different paths and querystring. This means that a page with url `/users?page=1` and `/users?page=2` are cached independently even though the same HTTP handler is being used by both URL.

The code of the middleware is as follow.

```go
package main

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"time"
)

func cached(duration string, handler func(w http.ResponseWriter, r *http.Request)) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {

		content := storage.Get(r.RequestURI)
		if content != "" {
			fmt.Print("Cache Hit!\n")
			w.Write([]byte(content))
		} else {
			c := httptest.NewRecorder()
			handler(c, r)

			for k, v := range c.HeaderMap {
				w.Header()[k] = v
			}

			w.WriteHeader(c.Code)
			content := c.Body.String()

			if d, err := time.ParseDuration(duration); err == nil {
				fmt.Printf("New page cached: %s for %s\n", r.RequestURI, duration)
				storage.Set(r.RequestURI, content, d)
			} else {
				fmt.Printf("Page not cached. err: %s\n", err)
			}

			w.Write([]byte(content))
		}

	})
}
```

To use it we just need to wrap our HTTP handler function inside a `cached` call, like the following. Thanks to Go's `time` package, we can use human friendly string to represent a duration. For instance, **10s** is much easier to understand than **10 * 1000**.

```go
  // both idex and about are: func (w http.ResponseWriter, r *http.Request) { ... }
	http.Handle("/", cached("10s", index)) 
	http.HandleFunc("/about", about)
  http.ListenAndServe(...)
```

On this example, only the index route is being cached.

Check out the following images of two subsequent calls to the same address.

First request takes **2 seconds** while second request is served in just **27ms** with the **exact same content and size**.

![](/public/images/2017/03/load-one.png)

![](/public/images/2017/03/load-two.png)

After 10 seconds from the first request, the next request will take longer again as the page cache has been expired.

### What you should take care when implementing server side cache

The first thing to do is **never** cache POST, PUT or DELETE requests as these are used to change resources and not retrieve data, so it doesn't make sense to cache it. That being said, only GET requests should be cached. **Tip**: It is possible to avoid mistakes like this by implementing a safe check on the middleware :)

Take an extra care for user based content. Applications and websites that requires user to be logged in can also be cached, but you'll need to take into consideration that the user identification have to be used as part of your key, otherwise you'd be returning content from one user to other users, which is definitely a huge data breach. The drawback of doing this is that you'll end up with much more cached pages. So keep this in mind when doing it.

And last, but not least, you definitely need to implement a feature switch to easily turn this off on development environment for obvious reasons :)

### Congratulations 

You just made something fast become even faster, thus helping make the Web a better place. Thank you!

If you like it, please share it or leave me a comment!

Cheers,
Guilherme