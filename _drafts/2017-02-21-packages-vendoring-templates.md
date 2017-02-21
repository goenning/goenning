---
layout: post
title: "Packages, vendoring and templates in Go"
comments: true
lang: en
tags: [go, golang, web, gostore]
series: firstwebappgo
ref: packages-vendoring-templates
---

On the last post of this series we finished with a fully working web app written in Go using only standard libraries. During this post you’ll learn how package management works in Go by adding a very famous third-party package. Besides that, we’ll also introduce some more advanced HTML template usage.

### Packages

Go projects are organized in packages. You can create your own packages, like the `main` which we created on previous post, or you can make use of third-party packages to speed up your development process. By default Go comes with what is known as standard library (or stdlib for short). We've used this before when we typed things like import `html/template` and `net/http`.

As Go is a compiled language, whenever you build a project that references some package, your code and all the source from those packages are fetched from local folders and compiled into a single native machine code executable. This means we really don't need any Go tooling installed on production environment, everything is contained into a single binary, which is awesome! 

Let's start making use of packages to organize our project.

Go doesn’t allow you to have two packages inside the same folder, that's why we have to create a folder for our new package and throw our code in there. Create an `env` folder and a `env.go` file inside it.

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

Different than some languages in which you have to explicitly reference what you want to import, Go only cares about the package name, so you can name your file whatever you want. Once a package is imported, you’ll have access to all of its exported objects.

Exported objects? Did I mention this before? No, right?

So this is another neat thing about Go. You don't need to explicitly export anything or set something as private or public, all you have to do is follow a convention. 

Everything that starts with capital letter is exported from that package. Whenever it's lowercase is considered internal to the package and can't be used outside of it. So in our example, GetEnvOrDefault is public because we need to use on out main package. Cool, isn't it?

Here's how our entry point looks like when using a package.

We can now change our entry point to use our new package and its function instead of doing it by itself.

```go
package main

import (
	"fmt"
	"html/template"
	"net/http"
	"github.com/goenning/gostore/env"
)

func main() {
  port := env.GetEnvOrDefault("PORT", "8080")
	http.HandleFunc("/", index)
	fmt.Printf("Server is up and listening on port %s.\n", port)
	http.ListenAndServe(":"+port, nil)
}
```

Note how we need to add full path to our `env` package. Curious why? Just keep reading :)

### Go and its Dependency Management

Go dependencies are managed in a totally different way when compare to platforms like .NET, Java and Node.js. Instead of an dependency registry, Go relies on public url that simply hosts some go files. These url usually point to a git repository, like GitHub and Bitbucket, but that's not a rule.  (Or is it@@@@???)

Al packages are imported by specifying full path starting from $GOPATH/src, which is why $GOPATH needs to be setup before using packages in Go. The only exception for this rule is stdlib(@@@?)

Let's start by doing something very simple. As we've seen before, Go's default router is not that flexible and has some drawbacks. We'll replace it with **gorilla/mux**, a battle tested and very popular router for Go.

You can install it by running following command on your terminal.

> go get github.com/gorilla/mux

What **go get** will do is download the source code from github and place the files into $GOPATH/src.

You can verify this by going to your $GOPATH folder and inspecting it.

You can now create any Go app that's needs to use github.com/gorilla/mux as a dependency. We can now change our HTTP server to use gorilla/mux, like this:

@@@ code

You can now build and run your go project just like before.

Cool!You know learned how to use third-party packages.

### Vendoring 

This project will work on your machine as long as you have runned **go get** locally to fetch the dependency. But how would we share this code between our coworkers/community and ensure that everyone has all dependencies fetched and, mostly important, the correct version of each dependency.

That's where vendoring kicks in. It's basically a feature introduced on Go 1.5 that allows Go apps to fetch dependencies not only from GOPATH, but also from a child folder named **vendor** inside each project.

This means that you can place your dependencies into your project's vendor folder instead of globally shared GOPATH. Go compiler will first look into vendor folder before trying to fetch something from GOPATH.

Then a whole bunch of community driven tool started to become available that basically allow applications to explicitly specify their dependencies in a file and restore them by running a single command.

Glide, govendor & godep are some of the most popular tools, they can be easily compared to NPM, Maven and Nuget, for example. 

While you can achieve mostly the same with any tool, we'll use Glide on this tutorial as I find it simpler and widely used tool.

### Using Glide

Glide is available on many platforms, just download and install based on whatever OS you're on https://github.com/Masterminds/glide/releases

You should then be able to run **glide version**@@@ on terminal.

You can now install any dependency to your project by running the same **go get** commands, but now replacing **go** with **glide**, like this.

> glide get github.com/gorilla/mux

When you run it, two things happens. Firstly gorilla/mux is downloaded into your vendor folder instead of GOPATH, secondly glide.yaml is created/updated@@@ to include your newly added dependency.

So now, whenever you share this code, anyone wirh glide installed can simply run **glide install** and run your app.

### Auto rebuilding our app with Gin

**Gin** is a CLI tool built in Go that runs your web app and make sure it's restarted everytime a Go file changes.

We can install this by running following command on your terminal.

> go get https://github.com/codegangsta/gin

As gin is a CLI package, it's then compiled into a binary file placed into $GOPATH/bin (this is why you need this path on your $PATH).

Gin (and many other application hosts, like Heroku) exposed an environment variable named PORT with the port number your app needs to listen to in order to work property. This is exactly how gin works, so make sure you didnt skip first section of this post where we changes our project to use GetEnvOrDefault.

Instead of Go run you can now just run **gin** and navigate to your project with Localhost:3000. Whenever you change a Go file, gin will reload your app and you won't need to run any command, just refresh the page and see the difference. It's a game changer for a better developer experience, you should definitely try it.

### Leveling up with html/template

- explain how packages import work, go get
- go get https://github.com/codegangsta/gin to watch and rebuild after file changes. talk about getPort
- explain how vendoring works, go 1.5+ and which tools are available
- install glide
- init glide with glide create, will create glide.yml
- install gorilla mux with glide get
- use gorilla mux on our app and run it, Listen with nil router uses -> http.DefaultServeMux
- structs and it's own packages
- mock some products and show a demo store page using templates with range
- add some ui packages with npm 
- gorilla mux static to hide node_modules
￼
{% include _series_firstwebappgo.html %}
