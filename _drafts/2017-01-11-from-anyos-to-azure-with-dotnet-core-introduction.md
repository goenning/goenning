---
layout: post
title: "From any OS to Azure with ASP.NET Core: Introduction"
comments: true
lang: en
tags: [dotnet, aspnetcore, azure, macos]
series: fromanyostoazure
cover: /public/images/2017/01/dotnet-logo.png
---

I've always been a big fan of the C# language and the .NET platform. But due to it's high dependency on Windows OS to be able to build and run .NET applications, I neved had a pleasurable experience when trying to code it on my macOS. The good news is that things have changed dramatically since then. Thanks to .NET Core, developers are now able to code in C#, on any OS and deploy to any Cloud.

The purpose of this tutorial is to give everyone a good starting point on how to create a web applications with ASP.NET Core from scratch, deploy it to the cloud with Azure and add all the latest cool technologies available out there to enrich our app.

### dotQuiz

I'd like to introduce you to **dotQuiz**, the application we'll building through this blog series. dotQuiz is a multi-language, multi-cultural quiz-like game that runs (or will run someday) on any browser or mobile phone. There will be lot's of different *game modes*, but we'll start it simple and enhance it step by step.

The back-end will be an API that will expose information and operations to interect with the game. This API will then be available for any other front-end application to consume it and build the UI for the game.

This is the stack I plan to use through this series.

- C# language 
- .NET Core platform
- xUnit for unit testing
- ASP.NET Core MVC as the web framework for our API

If possible, I'll be working on it and add the following:

- TypeScript and React for the Front-end
- MongoDB for storage
- Docker

The source code is available on [GitHub](https://github.com/goenning/dotquiz-api) and each post of this series will be tagged for proper history.

These are the tools I'll be using on this tutorial, make sure you have all oh them or a suitable replacement.

### macOS Sierra 

You're free to choose any other you prefer. <i class="fa fa-heart" aria-hidden="true"></i> .NET Core

### .NET Core Runtime

.NET Core it the new kid on the block available for any developer. It's a cross-platform, free and open source platform for running applications written in C#.

To build your own apps you'll need the .NET Core SDK which can be downloaded here [https://www.microsoft.com/net/core](https://www.microsoft.com/net/core). I'll be doing this with the .NET 1.1 version, even though it's not a LTS version, you can still download and use it as it's also a Production-ready version.

### `dotnet` CLI

When you install .NET Core SDK, the `dotnet` executable will be installed and available on your favorite terminal. `dotnet` CLI is the program used to build, run and publish your .NET applications.

### NuGet

NuGet is the Package Manager for .NET applications. It's available for both .NET Framework and .NET Core applications. You'll be able to fetch NuGet packages using the `dotnet` program.

Check out the [official NuGet](https://www.nuget.org/packages) website to see all available packages.

### Visual Studio Code

It's a free, open source, cross-platform and extensible IDE, you can [download it here](https://code.visualstudio.com/).

Even though you can use any IDE, I'd really recommend you to give it a try. I consider it the best free IDE for writing HTML, Markdown, JavaScript, TypeScript and now, C# :)

My recommended extensions are:

- [C#](https://marketplace.visualstudio.com/items?itemName=ms-vscode.csharp)
- [vscode-icons](https://marketplace.visualstudio.com/items?itemName=robertohuertasm.vscode-icons)

## Are you ready? Let's get started!


{% include _series_fromanyostoazure.html %}

Hope you enjoy it. If you do, please leave a comment.

Cheers!

{% include _series_fromanyostoazure.html %}