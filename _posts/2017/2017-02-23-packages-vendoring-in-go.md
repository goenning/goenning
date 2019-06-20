---
layout: post
title: "Packages and vendoring in Go"
lang: en
tags: [go, web]
series: firstwebappgo
ref: packages-vendoring-in-go
---

On the last post of this series we finished a simple, but fully working web app written in Go using only the standard library. During this post you’ll learn how package management works in Go by adding a very famous third-party package.

![](/public/images/2017/02/go-packages.jpg)

### Packages

Go projects are organized into packages. You can create your own packages, like the `main` that was created on previous post, or you can make use of third-party packages to speed up your development process. By default Go comes with what is known as standard library (or stdlib for short). We've used this before when we typed things like import `html/template` and `net/http`.

As Go is a compiled language, whenever you build a project that references one or more packages, your code and all the source code from those packages are fetched from local folders and compiled into a single native machine code executable. This means that we really don't need any Go tooling installed on production environment, everything is contained in this single binary, even the external packages.

Let's start making use of packages to organize our project.

Go doesn’t allow we to have two packages inside the same folder, that's why we have to create a folder for our new package and put our code in there. Create an `env` folder and a `env.go` file inside it.

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

Different than others languages in which you have to explicitly reference what you want to import, Go only cares about the package name, so you can name your file whatever you want. Once a package is imported, you’ll have access to all of its exported objects.

Exported objects? Did I mention this before? No, right?

So this is another neat thing about Go. You don't need to explicitly export anything or set something as private or public, all you have to do is follow a convention. 

Everything that starts with capital letter is exported from that package. Whenever it's lowercase is considered internal to the package and can't be used outside of it. So in our example, `GetEnvOrDefault` is public because we need to use it in our `main` package. Cool, isn't it?

We can now change our entry point to use our new package and its function.

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

Note how we need to add full path to our `env` package. Curious why? Just keep reading :)

### Go and its Dependency Management

Go dependencies are managed in a totally different way when compare to platforms like .NET, Java and Node.js. Instead of an dependency registry, Go relies on public url that simply hosts some Go files using a common VCS system like Git, SVN or Mercurial.

All packages are imported by specifying full path starting from `$GOPATH/src`, which is why `$GOPATH` needs to be setup before using packages in Go. The only exception for this rule is `stdlib`, which is imported from `$GOROOT/src`.

Let's start by doing something very simple. As we've seen before, Go's default router is not that flexible and has some drawbacks. We'll replace it with `github.com/gorilla/mux`, a battle tested and very popular router for Go.

You can get it by running following command on your terminal.

> go get github.com/gorilla/mux

What `go get` will do is download the source code from GitHub and place the files into `$GOPATH/src/github.com/gorilla/mux`. You can verify this by going to your `$GOPATH` folder and inspecting its tree.

You can now create any Go app and use `github.com/gorilla/mux` as a dependency. 

The following code is an example on how to change our HTTP server to use `github.com/gorilla/mux` instead of the default.

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

The only thing that has changed is that we now create a new router using `mux.NewRouter()`, register all handler into this router and use it to serve on given port. Remember that before we had a `nil` value as the second parameter? That's the `router` parameter, so when it's `nil`, it uses the default one.

You can now build and run your Go project just like before.

Well done! You know learned how to use third-party packages :)

### Vendoring 

This project will work on your machine as long as you have runned `go get` locally to fetch the dependency. But how would we share this code between our coworkers/community and ensure that everyone has all dependencies fetched and, mostly important, the correct version of each dependency.

That's where `vendoring` kicks in. It's basically a feature introduced on Go 1.5 that allows Go apps to fetch dependencies not only from `$GOPATH/src`, but also from a child folder named `vendor` inside each project.

This means that you can place your dependencies into your project's vendor folder instead of globally shared $GOPATH. Go compiler will first look into vendor folder before trying to fetch something from $GOPATH.

Then a whole bunch of community driven tool started to become available that basically allow applications to explicitly describe their dependencies in a file and restore them by running a single command.

Glide, govendor and godep are some of the most popular tools, they can be easily compared to NPM, Maven and NuGet, for example. 

While you can achieve mostly the same with any tool, we'll use Glide on this tutorial as I find it simpler and widely used tool.

### Using Glide

Glide is available on many platforms, just download and install based on whatever OS you're on [https://github.com/Masterminds/glide/releases](https://github.com/Masterminds/glide/releases).

You should then be able to run `glide -v`.

The first command that has to be run once for each new project is `glide init`, which will basically create a `glide.yaml` that describe all your project dependencies. 

As we have already imported `github.com/gorilla/mux` using `go get`, this command will discover it and already set it as a dependency. But you could just install this or any other dependency by running the same `go get` command, just replacing `go` with `glide`.

> glide get github.com/gorilla/mux

When this command is executed, two things happens. 

1. `github.com/gorilla/mux` is downloaded into your vendor folder instead of `$GOPATH/src`.
2. `glide.yaml` is updated with your newly added dependency.

```yaml
package: github.com/goenning/gostore
import:
- package: github.com/gorilla/mux
  version: ^1.3.0
```

So what you have now is a `glide.yaml` that describes all your project dependencies, so anyone with Glide installed can simply run `glide install`, compile and run your app.

There is also the `glide.lock` file, which is basically a lock file used by Glide to ensure that your project is always using the same version for each dependency whenever `glide install` is ran.

### Auto rebuilding our app with Gin CLI

`Gin` is a CLI tool built in Go that runs your web app and make sure it's restarted everytime a Go file changes.

We can install this by running by using `go get`.

> go get https://github.com/codegangsta/gin

As Gin is a CLI package, it's compiled into a binary file placed into `$GOPATH/bin`, which is why it's recommended to add this path on your `$PATH`.

Gin (and many other application hosts, like Heroku) expose an environment variable named `$PORT` with the port number that your app needs to listen to in order to work properly. This is exactly how gin works, so make sure you didnt skip first section of this post where we changes our project to use `GetEnvOrDefault`.

Instead of `go run` you can now just run `gin` on your terminal and navigate to `http://localhost:3000` to see your app running.

Whenever you change a Go file, Gin will reload your app and you won't need to run any command, just refresh the page and see the difference. It's a game changer for a better developer experience, you should definitely try it.

### Let's just recap what we've just learned:

1. How Go code is organized into Packages
2. How to manage your project dependencies
3. We can install CLI tools written in Go using a single command

That's all for today folks. Looking forward for the next post.

The source code of this project is still always available at [https://github.com/goenning/gostore](https://github.com/goenning/gostore).

If you have any questions or suggestions, please drop me a comment.

Cheers!
￼
{% include _series_firstwebappgo.html %}
