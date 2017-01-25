---
layout: post
title: Adding custom data to Go binaries at compile time
comments: true
lang: en
tags: [go, goland, ldflags]
ref: adding-custom-data-go-binaries-compile-time
---

Sometimes it is really useful to be able to get some information about your Go binaries, for example: when it was built it, which user compiled it, which git commit was used, etc. This information is specially useful for versioning your binaries, to troubleshoot bugs or just to have a reference to the source code.

How would you do it?

- We could create a `json/yml/xml` file to store this information. Yup, we could, but do you want to manually update it everytime? You'd also need to distribute two files instead of just one.
- What about a `version` global structure? Well, it'd be better as we would end up with just the binary and no additional configuration files. But even then, we'd need to update the structure manually before each build.

### Compiler flags to the rescue

Have you even heard of `-ldflags`?

This compiler flag is used by `go install|build` to override Go variables in our program.

So if we had this in our `main package`.

```
package main

var saySomething string
```

If we ran `go build -ldflags "-X main.saySomething=HelloWorld"`, the compiler would set `HelloWorld` to our `main.saySomething`. Wow!

We could also have something like the following Go code.

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

And build it like this.

> go build -ldflags "-X main.buildTime=$(date +"%Y.%m.%d.%H%M%S") -X main.commitHash=$(git log --pretty=format:'%h' -n 1)"

You'd end up if something like this:

```
Build Time: 2017.01.25.070111
Commit Hash: 1e4e7b5
```

Hey, there is no problem if you don't set this variables during build time as they would just remain with an empty value, ok?

### Working with packages

I could end this post here, but I can't let you go without knowing this.

As you can see on our previous example, we had to specify both the package (`main`) and variable name (`buildTime` and `commitHash`). This means that we can also use this flag to inject variables into packages! Hmm, show me the code!

Ok. 

If we have the following package.

```
package info

var (
	BuildTime string
	CommitHash string
)
```

We could then change it by using the full package path, like this:

> go build -ldflags "-X github.com/goenning/hello-go/info.BuildTime=$(date +"%Y.%m.%d.%H%M%S") -X github.com/goenning/hello-go/info.CommitHash=$(git log --pretty=format:'%h' -n 1)"

Just remember that as we are writing a package that is intended to be imported somewhere else, our variables need to start with a capital letter. You could also implement some sort of encapsulation to have access to them from outsite your package.

### That's all for today folks

I hope this tips can help you build your binaries with richer information and save you from bug hunting heachaches ;)