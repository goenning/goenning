---
layout: post
title: 'MiniProfiler for SharpSapRfc: A new NuGet Package'
comments: true
tags: [sap, miniprofiler, performance, .net]
---

### MiniProfiler

It has been a long time since I first heard about MiniProfiler, but I never had the opportunity to use and study it. Recently — because of some performance tuning tasks I had to do — I started to use it and I enjoyed so much that I decided to share my experience here.

It's all about a tiny package, easy to install and configure, that brings to the eyes of the programming some really useful information regarding response time of each HTTP Request. It was created by [Stack Overflow](http://stackoverflow.com/) team and it is used by them in production. Who needs a better success case than this, right?

With MiniProfiler it is possible to know how much time was taken in each step of the pipeline, from the beginning until the end of the server processing. Leave it enabled in your development environment and for each every HTTP request you'll see in the top left side of your page an small box with timing information about the request.

![](/public/images/miniprofiler-demo.png)

### Example of profiling with MiniProfiler

It is necessary to add a call to MiniProfiler on each peace of code that you may need to record the execution time. There are a few packages in NuGet that triggers automatic register some profiling steps without having to code anything.

One of these package is `MiniProfiler.MVC4` that will record the execution time for all controllers calls and view rendering. Amazing, isn't it?

Web applications normally does heavy use of external resources like SQL Databases, NoSQL Databases, WebServices and such. Bad usage of these resources are usually guilty for major application slowness and it is really hard to troubleshoot and track things like this by the eye. There are also MiniProfiler packages for most of these resources like SQL (any database provider), Entity Framework, MongoDB, Raven and, more recently, ...

### MiniProfiler.SharpSapRfc

Remote calls to SAP functions may be slow, especially when dealing with standard functions (known as BAPI). Using a profiler helps troubleshooting the guilty for a slow HTTP request that makes many SAP calls.

For this matter I've created a package named `MiniProfiler.SharpSapRfc`. With this package it is possible to record all Remote Functions Call made with [SharpSapRfc](https://github.com/goenning/SharpSapRfc). The source code is available at [goenning/MiniProfiler.SharpSapRfc](https://github.com/goenning/MiniProfiler.SharpSapRfc) and binary is published at [NuGet](https://www.nuget.org/packages/MiniProfiler.SharpSapRfc/).

Instructions for install and how use are available at project's GitHub homepage.

Below are some demo images I've made while developing. The profiler works for both SharpSapRfc.Soap and SharpSapRfc.Plain (x64/x86).

![](/public/images/miniprofiler-demo-sap-1.png)
![](/public/images/miniprofiler-demo-sap-2.png)

Cheers!