---
layout: post
title: How to retry when React lazy fails
lang: en
tags: [react]
description: React 16.6 has been released and it's now easier than ever to do code split within our React applications by using the lazy function. After a few days monitoring a production application that is using lazy, I noticed a couple of client-side errors when downloading asynchronous modules. Learn how to mitigate this.
ref: retry-react-lazy-fails
---

React 16.6 has been released and it's now easier than ever to do code split within our React applications by using the lazy function.

If you don't know what I'm talking about, you should definitely read this [https://reactjs.org/blog/2018/10/23/react-v-16-6.html](https://reactjs.org/blog/2018/10/23/react-v-16-6.html)

After a few days monitoring a production application that is using lazy, I noticed some client-side errors like this:

```
Loading chunk 6 failed. (error: https://.../6.4e464a072cc0e5e27a07.js)
Loading CSS chunk 6 failed. (https://.../6.38a8cd5e9daba617fb66.css)	
```

**Why?!**

I don't actually know why, but I can only assume this is related to the user network. Maybe they are on a slow 3G and there was a network hiccup? That's not a rare event, right?

Alright, if that's true, how do we solve that?

We can do the same thing that everyone does when a network request fails: retry it! 😄

**How?**

Did you know that the **import(...)** function that we use on lazy is just a function that returns a Promise? Which basically means that you can chain it just like any other Promise.

Below you can find a basic implementation of a retry function.

```javascript
function retry(fn, retriesLeft = 5, interval = 1000) {
  return new Promise((resolve, reject) => {
    fn()
      .then(resolve)
      .catch((error) => {
        setTimeout(() => {
          if (retriesLeft === 1) {
            // reject('maximum retries exceeded');
            reject(error);
            return;
          }

          // Passing on "reject" is the important part
          retry(fn, interval, retriesLeft - 1).then(resolve, reject);
        }, interval);
      });
  });
}
```
> Source: [https://gist.github.com/briancavalier/842626](https://gist.github.com/briancavalier/842626)

Now we just need to apply it to our lazy import.

```javascript
// Code split without retry login
const ProductList = lazy(() => import("./path/to/productlist"));

// Code split with retry login
const ProductList = lazy(() => retry(() => import("./path/to/productlist")));
```

If the browser fails to download the module, it'll try again 5 times with a 1 second delay between each attempt. If even after 5 tries it import it, then an error is thrown.

That's all! 🎉

Thanks!