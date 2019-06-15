---
layout: post
title: MiniProfiler for Node.js is back in business
lang: en
tags: [node.js, miniprofiler, performance]
---

I'm truly addicted to [MiniProfiler](http://miniprofiler.com). Have being using it in my ASP.NET projects for a while and it really shines. I love to be able to see exactly what is making a particular request become slow or to find those nasty SELECT N+1 issues. I/O operations is the top reason for a slow application and MiniProfiler helps us find the bottleneck with ease.

Now that I'm working mostly with Node.js, I felt the need of using it as well. I found a port of it, which is hosted at GitHub under [MiniProfiler/node](http://github.com/miniprofiler/node), but it was untouched for the last 2 years.

I decided to give it a try and take it over.

![](/public/images/2016/05/28/miniprofiler-1.png)

I'm now actively working on the first releasable version of it. There is a still a lot of work to do, the API may change, but there was some progress so far.

- as an avid TDD practitioner, I've included unit tests that is currently [covering 98% of the code](https://coveralls.io/github/MiniProfiler/node);
- removed all the utility functions and replaced with underscore, which was already a dependency. Let's not reinvent the wheel, right?
- setup of a linter so that we now have a standard coding style to follow;
- added support for all major Node.js web frameworks, which includes [express](http://expressjs.com), [koa](http://koajs.com), [hapi](http://hapijs.com) and [raw http](https://nodejs.org/api/http.html) as well;
- updated [ui](http://github.com/miniprofiler/ui) to the latest version;

The next big step is to start writing providers to instrument the most used I/O libraries like [pg](https://www.npmjs.com/package/pg), [redis](https://www.npmjs.com/package/redis), [mongoose](https://www.npmjs.com/package/mongoose) and much more.

We will then be able to profile an entire Node.js app by doing a simple setup at startup and not changing anything else inside the app.

The real challenge is how to do that. I know it's possible because there's a hell out of good libraries doing instrumentation in JavaScript. But to me that's a whole new world.

Aside from that, there's also some other improvements and features yet to be done, like:

- storing client-side timing;
- storage should be asynchronous
- deliver some out of the box storage options like Redis and MongoDB;
- some real world example of apps using it;
- document everything, inside code and at README;
- include TypeScript typings;
- setup a docker composer so that developers won't need to install anything locally to start making MiniProfiler awesome (Postgres, MongoDB, Redis, etc.);

I you like MiniProfiler as much as I do and you're interested in helping, I would appreciate any feedback on what we've being doing so far and on the roadmap as well, just drop us an issue at [GitHub](https://github.com/miniprofiler/node/issues) or leave a comment. Pull requests are also always welcomed!


![](/public/images/2016/05/28/miniprofiler-2.png)