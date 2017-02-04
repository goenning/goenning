---
layout: post
title: "Your very first web app using Go"
comments: true
lang: en
tags: [go, golang, web, gostore]
series: firstwebappgo
ref: your-very-first-web-app-go
---

Today I’m starting a new blog series on how to build your first web application using Go. I’ll do it by following a baby steps approach, so you can expect a long blog series covering everything you need (or should) do/use to build a web application, like Go tools, GOPATH, test, mock, vendoring, database and, of course, hosting in the cloud.

Are you ready?

### Ok. Let’s Go!

The app we're going to build **together** is called **Go Store**. 

The purpose behind this app is to enable any developer to create their own web store. There will be no shopping cart, checkout or payment process, it's only about creating a page that can be used to show everything it's available to sell and how to get in touch.

I recently moved abroad and because of that I had to set up a very simple static website with everything I had to sell before moving. 

If I had done this app before, I could have used to host my own dynamic store instead of having to change a HTML file everytime I had to update a product.

The source code of this project will be always available at [https://github.com/goenning/gostore](https://github.com/goenning/gostore).

### Installation 

The first thing you’ll need to do is, of course, install Go. If you haven’t done it yet, just go to [https://golang.org/doc/install](https://golang.org/doc/install) and follow the guide. Make sure you follow all steps as Go installation is not just next, next, finish, you'll have to set up some environment variables

### Your very first Go web app 

The first step is to set up our project structure. 

On my machine I'm using `$GOPATH/src/github.com/goenning/gostore` as my root project folder. Feel free to use whatever you want, as long as it's inside `$GOPATH/src/`.

Once you've done that, create a `main.go` file and paste the following code. This is all you need to create a very simple Go web app. 

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

‘main‘ is the entry point for our program, so it's the first function to be called when running our app.

`http` is a package of Go standard library that is used to create HTTP servers and clients. In this example we are creating a server when we call `ListenAndServe` passing a port to listen.

But before creating the server, we need to configure all routes we want our app to be able to handle. 

To do so we call `http.HandleFunc` passing two arguments. The first argument is the path we want to match, while the second is the HTTP handler that will be called once the incoming request URL matches the pattern.

Our handler is very simple. We first load a template called `index.html`, we then render it with a data structure into our ResponseWriter and set the status code of the response to 200. Both the content and status code are then sent back to the client.

For the sake of brevity, I have completely ignored any possible error that could happen on `ParseFiles` and `Execute`. This is not a good practice at all as we should always handle errors accordingly, but I'll leave it for later when we come back and refactor this code.

Here is our template file called `index.html`.

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

You can see we have some variables inside double curly braces. These variables are replaced with whatever you pass on seconds argument of `tpl.Execute`.

You can test this application by running `go run main.go` in your favorite terminal and navigate to `http://localhost:8080/` with any browser. 

If everything went well you should see a basic welcome message printed on screen. 

Yes, that's all you need! No Apache, Tomcat or IIS. Go's built-in http server can do it without any additional tool.

The interesting part is that you can navigate to **any** page on this address and you will still see this very same message, even though our handler matcher is just `/`. 

This might seem awkward to some, but it's just how Go's default Mux works. You can get some more information about it on [official docs page](https://golang.org/src/net/http/server.go?s=57308:57433#L1890). 

If you don't want this behavior you can easily create your own Mux or use any open source alternatives, there are dozens of them. We'll take a look at one of them in the near future.

### Let's just recap what we've learned so far:

1. `http` is a standard Go library that can be used to create both HTTP servers and clients.
2. We don't need any additional web container to host a web application written using Go.
3. Go's default Mux is easy to use, but has some drawbacks.

That's all for today folks. Looking forward for the next post where we'll introduce a custom mux, mocked data and some front end stuff to make our page more user friendly.

If you have any questions or suggestions, please drop me a comment.

Cheers!
￼
{% include _series_firstwebappgo.html %}