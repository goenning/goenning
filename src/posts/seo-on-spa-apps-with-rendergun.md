---
slug: seo-on-spa-apps-with-rendergun
title: Adding SEO to Single Page Applications with Rendergun
tags: [spa, seo]
publishedAt: "2019-11-05T00:00:00Z"
description: "Learn how to add SEO capabilities to Single Page Applications using Dynamic Rendering and Rendergun."
---

SPA and SEO are two acronyms that don’t fit well together on the same sentence. Even though Google is able to crawl and index client-side rendered websites since 2015, there are a few reasons why this is not yet recommended.

On this post I’ll take you though an alternative solution on how to make a SPA page SEO-ready using a technique called “Dynamic Rendering” and an open source project called Rendergun.

## What is the problem of SEO on a Single Page Application?

[This post from 2015](https://webmasters.googleblog.com/2014/05/understanding-web-pages-better.html) mentions that Google is able to download, parse and execute JavaScript that generates the DOM, hence why client-side rendered applications are indexable.

But this [video series](https://www.youtube.com/playlist?list=PLKoqnv2vTMUPOalM1zuWDP9OQl851WMM9) explains that executing JavaScript is resource intensive operation because a real browser is used. Google currently queues client-side applications rendered to be processed with a much lower priority compared to server-side rendered applications.

Keep in mind that Google is indexing the whole internet and every day new pages are being discovered. When will they have enough resources to index our SPA? Every time the SPA has new or updated content, it can take weeks for Google to index it.

A common solution is to use a server-side rendering feature of a JavaScript framework. But sometimes this can be complex to implement if not taken into consideration when starting the project.

Dynamic Rendering is an alternative solution that can help us get better SEO results even on a Client-Side rendered application.

## What is Dynamic Rendering?

A normal user cannot easily differentiate between a server-side and client-side rendered application. The reason being that the user’s browser is able to easily process both kinds of response and display the UI regardless of where it’s rendered.

On the other hand, web crawlers (or bots) like Google Search, Bing, Yandex and many others cannot easily process JavaScript, they’ll always prefer a server-side rendered response with big HTML body over a thin HTML and loads of JavaScript code.

What if we could detect whether the request is coming from a bot and then return a server-side rendered version of the page only for crawlers? Normal users can still download and execute the JavaScript version and render it directly on their browser.

This image from google is a good visual representation of what I just explained above:

![](/blog/seo-on-spa-apps-with-rendergun/how-dynamic-rendering-works.png)

_Source: [Google Developer Docs](https://developers.google.com/search/docs/guides/dynamic-rendering)_

The traditional server-side rendering usually requires some changes on the code to ensure that every component is able to render on both server and client. But there’s another approach on server-side rendering that is language and framework agnostic. It’s called “prerender” and it fits very well into the Dynamic Rendering concept.

Remember that we mentioned that Google has a low priority queue for JavaScript rendered pages due to the resource intensive operation?

What if we could offload this task from them and do it ourselves? We can do exactly the same thing that they do. This is commonly known as “prerender”.

## Prerendering

Prerender is the technique of using a headless browser on the server to download, parse and execute the JavaScript, which then builds the DOM that is converted it into a string and returned to the client.

This technique is language agnostic, the only requirement is being able to communicate with a headless browser.

It’s also framework agnostic, because the headless browser will execute the JavaScript like any other browser, it doesn’t matter whether the application using React, Angular or Vue, for example.

But integrating with a headless browser can be a complicate process too. For performance reasons, it’s often recommended to reuse same instance rather than a new one per render process. Browsers will often crash, so we also need to monitor its health and possible restart it if necessary. It’s also common for browsers to misbehave after being open for many hours, so we should recycle the instances every once in a while.

Fortunately, there are easier ways to do it.

## Enter Rendergun

Rendergun is an open source web server that exposes a Web API interface to perform prerendering of JavaScript rendered pages.

We can start rendergun as a global NPM package or as a container.

```bash
# install rendergun install NPM
$ npm i -g rendergun

# start rendergun with default configuration
$ rendergun
```

OR

```bash
docker run --name rendergun -p 3000:3000 goenning/rendergun
```

We now have rendergun running locally on port 3000.

Let's use [Fider](https://getfider.com) as an example of a client-side rendered website. When we open `https://feedback.fider.io` on a browser, it'll download a few javascript files and render the page on the client using React.

But when we open `http://localhost:3000/render?url=https://feedback.fider.io` on a browser, rendergun will do the same operation using its headless chrome instance and return the rendered page as HTML. We can actually compare the response from both websites.

The image below is the response from `https://feedback.fider.io`, note the body with empty divs and 4 JavaScript files.

![](/blog/seo-on-spa-apps-with-rendergun/fider-client-side.png)

Compare it with this one from `http://localhost:3000/render?url=https://feedback.fider.io`, note the body with the whole HTML content and no JavaScript files.

![](/blog/seo-on-spa-apps-with-rendergun/fider-server-side.png)

When integrating rendergun into SPA, a thin server is required so that we can intercept crawler requests and handle it differently. The pseudocode would look something like the following.

```javascript
if isCrawlerRequest() {
    var response = httpClient.get('<rendergunURL>/render?url=' + request.url)
    return response.body;
}

var file = readFile('index.html')
return file.content;
```

We would basically check if the request is coming from a crawler by looking at the `User-Agent` request header.

- If that's true, ask rendergun to render the current page and return the result, which is the server side rendered HTML;
- If it's not a crawler, then simply return the index.html which will then reference all the javascript files and will render on the client;

To learn more about rendergun, all available parameters and configuration, visit the official repository at [https://github.com/goenning/rendergun](https://github.com/goenning/rendergun).

## When should I use Prerendering over traditional Server-Side Rendering

The only reason to do Prerendering over traditional Server-Side Rendering is if we want to add SEO capabilities to an already existing SPA or if there is a server-side application that is not Node.js, which makes server-side rendering much more difficult.

When starting a completely new SPA project in which SEO is important, consider using Node.js and doing SSR with the chosen framework since day one.

## Conclusion

On this post we learned how to prerender a SPA using Rendegun and Dynamic Rendering.

Prerendering is a useful alternative solution to traditional SSR and we should take into consideration the pros and cons before applying it.

Rendergun gives us the peace of mind of not having to manage and monitor headless browsers instances, while giving us an easy to use HTTP interface.

Till next time!
