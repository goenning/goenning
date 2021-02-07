---
layout: post
title: How to set up a Cookieless Google Analytics website
lang: en
tags: [privacy, googleanalytics, cookieless]
description: There is a lot more awareness on privacy these days and quite a few solutions that are cookieless. Google Analytics is notorious for using Cookies, but did you know that you can actually configure it to be Cookieless as well? This post has everything you need to get started.
---

> **Note 1**: Some might argue that if you are concerned about cookies you shouldn’t be using Google Analytics in the first place. That’s a valid point in which I agree, but on this post I want to leave that discussion out. The focus here is to explain how to implement Google Analytics without cookies while still keeping an acceptable unique user metrics.

> **Note 2**: This post is using Universal Analytics from Google and it DOES NOT WORK with Google Analytics 4. I tried many times and I could not make it work with GA4, if you know how, please share!

It might be useful for us to start with why cookies are needed in Google Analytics. Without cookies the script is not be able to identify if the user visited that site before, which would cause all page views to be counted as unique. The bounce rate would also be severally impacted. 

To solve that, Google Analytics scripts creates a cookie on every user machine during the first visit. On a subsequent visit, the scripts can use that information to know if that's a new or returning visitor.

Thankfully there are solutions for this that do not rely on Cookies.

## First step: No more cookies

We need to tell GA to not store anything client side. The way we do it is via the `gtag` function.

```js
gtag('config', TRACKING_ID, { client_storage: 'none', anonymize_ip: true })
```

That’s it! We’re done. No more cookies.

## Second step: Unique user tracking

The code above is enough to remove cookies, but it doesn’t solve the unique user tracking problem. As mentioned before, every visitor will be counted as a new user.

The same `gtag` function above supports an additional property called `client_id`. Instead of letting GA generate a client id and storing on a cookie, we can generate one ourselves and send them. 

But after generating an ID, where do we store it? Most public websites do not require users to be logged in, so it's not possible to have a unique id per user as visitors are often anonymous.

One option is to use an algorithm that generates an ID based on the information available on the browser that could **potentially** identify a user. The following example is a function written in Node.js that generates a user ID based on their Browser, IP Address and Language. We can also add the current week to get a distinct ID every week even if all other properties have not changed. 

A week is probably enough to get meaningful unique user visits like return users, but depending on your needs, it’s possible to use other values, like the Month or Day.

```js
import crypto from 'crypto';

const currentWeekNumber = () => {
  const today = new Date();
  const firstDayOfYear = new Date(today.getFullYear(), 0, 1);
  const pastDaysOfYear = (today - firstDayOfYear) / 86400000;
  return Math.ceil((pastDaysOfYear + firstDayOfYear.getDay() + 1) / 7);
}

const md5 = (input) => {
  return crypto.createHash('md5').update(input).digest("hex");
}

export default (req, res) => {
  const clientIP = req.socket.remoteAddress
  const week = currentWeekNumber()
  const userAgent = req.headers['user-agent']
  const acceptLanguage = req.headers['accept-language']
  const host = req.headers.host
  const clientIDSource = `${clientIP}|${host}|${userAgent}|${acceptLanguage}|${week}`

  res.status(200)
  res.setHeader('Content-Type', 'text/javascript')
  res.send(`user_id='${md5(clientIDSource)}'`)
}
```

The function above generates a unique ID based on some information available to the server and write it to a global variable named `user_id`. It’s important to note that the function above MUST execute on a server, because JavaScript on the browser does not have access to its network IP address. Although the example above is running on Node.js, the same can be achieved in any language and framework.

## Last step: Connecting the dots

Something quite easy to do (which doesn’t mess up with `Content-Security-Policy`) is to add a script tag right before GA tag:

```html
<script src="/api/user"></script>
<script async src="https://www.googletagmanager.com/gtag/js?id=YOUR_TRACKING_ID"></script>
```

The code above assumes that a `GET /api/user` maps to that Node.js function above. The `user_id` variable can then be accessed later when calling `gtag`.

```js
gtag('config', YOUR_TRACKING_ID, { client_storage: 'none', anonymize_ip: true, client_id: window.user_id })
```

## Live Example

Here's a live example [https://cookieless-ga.vercel.app](https://cookieless-ga.vercel.app) using the method above which is also open source [https://github.com/goenning/cookieless-ga](https://github.com/goenning/cookieless-ga).

That’s it!