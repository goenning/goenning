---
layout: post
title: "From any OS to Azure with ASP.NET Core: A minimal setup"
comments: true
lang: en
tags: [dotnet, aspnetcore, azure, macos]
---

{% include _series_fromanyostoazure.html %}

First of all, check if `dotnet` CLI is installed correcly, because you'll need that.

Open your terminal and type `dotnet --version`. If everything is OK, you should get a response like `1.0.0-preview2-1-003155`.

The source code of this blog post is available as [v0.1 on GitHub](https://github.com/goenning/dotquiz-api/tree/v0.1). Go get if you want to clone or just look at it.

### The project.json file

`project.json` is a major definition file used to set compiler settings, dependencies, runtime frameworks and so on. It can be compared to `package.json` from Node.js projects or `Gemfile` from Ruby.

For our simple API we can start with the following API

```json
{
  "version": "1.0.0-*",
  "buildOptions": {
    "debugType": "portable",
    "emitEntryPoint": true
  },
  "dependencies": {},
  "frameworks": {
    "netcoreapp1.1": {
      "dependencies": {
        "Microsoft.NETCore.App": {
          "type": "platform",
          "version": "1.1.0"
        },
        "Microsoft.AspNetCore": "1.1.0",
        "Microsoft.AspNetCore.Mvc": "1.1.0",
        "Microsoft.AspNetCore.Server.Kestrel": "1.1.0"
      },
      "imports": "dnxcore50"
    }
  }
}
```

The important bits here are the list of packages that we require to build and run our app. 

- `Microsoft.NETCore.App` is the base package necessary to run any .NET application.
- `Microsoft.AspNetCore` is the base package for building web applications on .NET.
- `Microsoft.AspNetCore.Mvc` is the package that enables us to the MVC pattern.
- `Microsoft.AspNetCore.Server.Kestrel` is a lightweight web server, so we don't depend on any external server like Tomcat, IIS and so on.

### The entry point

Have you ever heard of `public static void Main(string[] args)`? So yeah, you'll now be using this for web applications as well, not only console or desktop UI.

Our API entry point is pretty simple.

```csharp
namespace DotQuiz.Api
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var host = new WebHostBuilder()
                .UseKestrel()
                .UseStartup<Startup>()
                .Build();

            host.Run();
        }
    }
}
```

We are basically setting `Kestrel` as our web server and the `Startup` class as our application configuration builder. 

```csharp
namespace DotQuiz.Api
{
    public class Startup 
    {
        public void ConfigureServices(IServiceCollection services) 
        {
            services.AddMvc();
        }

        public void Configure(IApplicationBuilder app, ILoggerFactory loggerFactory)
        {
            app.UseMvc();
        }
    }
```

This is the minimal configuration you need on your `Startup` class. The `Program` class doesn't change often, but `Startup` will certainly change a lot as your application evolves.

> What does **AddXYZ** and **UseXYZ** means? What is **IServiceCollection** and **IApplicationBuilder**?

You'll see a of this methods on your **Startup** class soon and it's important to know what's the difference and what they do.

**IServiceCollection** is the built-in Dependency Injection container for .NET Core applications, so you don't need any external DI library to do it for you (but you can if you want to change it). Packages that enhance ASP.NET Core will have a **AddXYZ** method to inject services into the DI container. In this case, **AddMvc** is injecting multiple services for various purposes into the DI container. On the next part of this series we'll add some more and show some neat ther features related to this.

**IApplicationBuilder** is where we configure our application's request pipeline. ASP.NET Core is using a middleware based pipeline and this is where we can attach middlewares to it. So keep in mind that the order of the methods call here are extremely important as the middlewares are going to be called in this particular order. As you might have guessed, **UseMvc** adds a routing engine to the pipeline that tries to match with available `Controllers` based on it's configured routes. We'll see this in action really soon.


### Our first Controller

```csharp
namespace DotQuiz.Api.Controllers
{
    [Route("api")]
    public class HomeController : Controller 
    {
        private IHostingEnvironment env;
        public HomeController(IHostingEnvironment env)
        {
            this.env = env;
        }

        [HttpGet, Route("values")]
        public IActionResult Index() 
        {
            return Ok(new {
                now = DateTime.UtcNow,
                env = env.EnvironmentName,
                values = new[] {
                    Guid.NewGuid(),
                    Guid.NewGuid(),
                    Guid.NewGuid()
                }
            });
        }
    }
}
```

The controller layout is pretty easy to understand. But there are some parts I'd like to explain.

The first one is the `IHostingEnvironment env` dependency in our `HomeController` constructor. This is being injected by the ASP.NET Dependency Injection and can be used in any controller that you need to have access to current environment data. We don't need this for this controller, but this is just to demonstrate one of the options to get services from your DI container into your controllers.

The second one is the `Ok(...)` method. The `Controller` base class defines a list of methods that you can use based on the HTTP Status code that you need to return. `Ok` returns a HTTP response with status code 200, while `NotFound` returns 404 and so on. For a full list of available methods, [check the official docs](https://docs.microsoft.com/en-us/aspnet/core/api/microsoft.aspnetcore.mvc.controllerbase#Microsoft_AspNetCore_Mvc_ControllerBase).

### All set. Let's see if it works!

To run your app, open your terminal, navigate to the your project folder and run `dotnet run`.

If everything went well, you should see the following output.

![](/public/images/2017/01/dotnet-run.png)

The default port for .NET applications is `5000`, so you should be able to see our controller being executed when we navigate to `http://localhost:5000/api/values`. The response will be something like this.

```json
{
  "now":"2017-01-12T21:19:56.761924Z",
  "env":"Production",
  "values":[
    "ce09b36f-9410-42b0-9a30-d8cb57fc7622",
    "6121d420-b549-4023-a934-2235f58b5512",
    "f14ed053-a675-4538-b571-f60154cc8d0f"]
}
```

### Well done!

Congratulations! This is the first to build our API and deploy it to the cloud.

Hope you enjoy it. If you do, please leave a comment.

Cheers!