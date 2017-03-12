---
layout: post
title: "Server side cache with Go"
comments: true
lang: en
tags: [go, golang, web, performance, cache]
cover:???
---

Golang is fast and everyone knows that. But how we can we make it ever faster when running a web applications on it?

When talking about speed, discussions usually end up with some sort of caching strategy. On this post I'll try to cover how can we achieve an even better response time for web applications using server side cache, because you know, performance **IS** a feature and I don't know anyone that enjoys spinning wheels.

### What's server side cache?

Most web developers are familiar with browser cache, which is basically how the server instruct the browser on how to cache a particular resource and for how long. This is achieved by using HTTPS headers like @@@. This is extremely useful and you should definitely use it for static assets like Javascript, CSS and I Images.

But what about HTML pages? How do you cache them? It's certainly not useful to cache it on the browser, because if your page changes, the user won't see the new content. Let's for instance think about a news portal like CNN, how do you serve the home page for millions of people so fast? If a new article is published, everyone needs to see it on the next refresh. 

That's where server cache comes into play. Building the index page of a news portal possible requires multiple IO operations, like database queries or API calls. The thing is, after the HTML of the index page is built for a user, it's possible to cache on the server and use this cached version to respond to all subsequent requests to the same page. By doing this on the server, we have full control on when to invalidate a given set of cached content when a new article is published, for example.

### How do we do it in Go?

Go is not just fast, it's also easy, and thus implementing this is not different.

The difficult part is to decide where you want to cache the page. Common strategies are usually to store it in memory, disk or a database. Either of these approaches are fine, but understanding the drawbacks of each of them is important to make a decision.

**In Memory**: Every page is cached on your web application's process memory, which makes it an excellent candidate for the fastest cache you'll ever have and the easiest to implement. The drawback is that if you have multiple servers (which you should probably have), you'll end with N copies of these cached content. If the process restarts for any reason, it'll lose all the cached content and thus slowing down the first request again.

**Disk**: Cached pages are stored on a disk. This may not be the fastest option available as the server would still need to read from disk and possible do some network operations if it's not a local disk. The biggest advantage is that it's easier to manage and very fault tolerant as they can survive reboots. 

**Database**: Cached pages are stored in a database, it could be SQL or a key-value storage, doesn't matter. The fact is that Redis is the king on this land. It's a high performance and battle tested in memory database widely used. It's not as fast as In process memory as it requires a network call, but content are shared across all servers and thus are not duplicated, consuming less hardware resources from the application server.

### Talk is cheap, show me the code

I'll show how to do it with in process memory, but I'll leave a full working example on the GitHub on how to do the same with Disk and Redis.

@@@ use interfaces. Show multiple pages, show time of cached, pages should sleep to simulate some slowness

### What you should take care when implementing server side cache

The first thing to do is never cache a POST, PUT or DELETE requests as these are used to change resources and not retrieve then, so it makes no sense to cache it. That being said, only GET requests should be cached.

Take an extra care for user based content. Applications and websites that requires user to be logged in can also be cached, but you'll need to take into consideration that the user identification have to be used as part of your key, otherwise you'd be returning content from one user to other users, which is definitely a huge data breach. The drawback of doing this is that you'll end up with much more cached pages. So keep this in mind when doing it.

And last, but not least, you definitely need to implement a feature switch to easily turn off this on development environment for obvious reasons :)

### Congratulations 

You just made something fast become faster. You're helping making the Web a better place. Thank you!

Cheers,
Guilherme

@@@pictures, gifs