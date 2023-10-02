---
slug: one-liner-immutable-mistake
title: The one-liner immutable mistake
publishedAt: "2023-10-02T00:00:00Z"
---

I recently received a [bug report](https://github.com/aptakube/aptakube/issues/155) where the user reported our app would freeze for a few seconds while loading data from an external API.

This surprised me, because all network operations happen in the background, keeping the UI fully responsive until the response is ready. Performance is a major selling point for this app, so I was keen on finding the root cause.

### Some context to set the scene

This app is a [Desktop Client for Kubernetes](https://aptakube.com/) built with Tauri. I also recently blogged about why [Tauri instead of Electron](https://aptabase.com/blog/why-chose-to-build-on-tauri-instead-electron) in case you’re interested.

Being built in Tauri means that the app shell is written in Rust, while the UI is a web view running HTML/CSS/JavaScript. We do all heavy lifting and external connections from the Rust side for performance reasons, only transferring whatever the UI requires for presentation purposes.

As most objects in Kubernetes tend to be fairly static, we also heavily cache them on the UI to improve the user experience. When users first view a list of objects, they get cached so that any subsequent render would read from cache first and feel instant, instead of having to wait for an API request to complete.

As the cache can become stale, we perform a request on the background to look for new changes and merge them back onto the UI if needed, while updating the cache as well.

### So what’s the issue here?

In this particular instance, the user found a noticeable app freeze when listing 5000+ objects from the cache. That surprised me as 5k is not a significant number of rows, it should all be instant just like when there are only hundreds of objects.

As this was a UI freeze and being sure that all network activity happens in the background, I knew something was going on in our JavaScript code.

As a pro frontend developer myself, I started the diagnostics using the most advanced performance tooling available on JavaScript: `console.time` and `console.timeEnd`, because why would I need anything else?

I’ve wrapped every function or code block with timing instructions looking for any function that was taking more than a few milliseconds to complete.

After a few minutes of copying and pasting console timers everywhere, I eventually found the culprit.

```javascript
const uidMap = objects.reduce(
  (acc, obj) => ({ ...acc, [obj.uid]: obj.resourceVersion }),
  {}
);
```

This used to be a one-liner, but I’ve split it into multiple lines for readability here.

When I first learned React back in 2017. One of the most important things I learned during that time was the importance of immutability in React and the existence of the spread operator. I’ve got used to using it everywhere, especially on redux reducers and within react components. My brain was so used to making everything immutable that I ended up using it even when it was not needed!

Fast forward to today, I’m not even using React for this app and I still constantly use the spread operator pretty much everywhere as you can see from the snippet above.

### So what’s the actual problem with that code?

This code block essentially creates a map of all object versions keyed by their unique ID. This is what we use to compare the cached state with the remote state to look for any object that may have a newer version.

We have an array of objects, but searching is not efficient as it’s `O(N)`, so we turn it into a map to benefit from the `O(1)` lookup operations.

The problem is that each iteration within the reduce function creates a new copy of the map and then inserts a new entry. In other words, each loop creates an array with “i” elements in it and then throws it away on the next iteration.

At the end of the loop, we have 4999 objects of 1 to 4999 items each for the garbage collector to clean! It’s not difficult to see how bad this is and how exponentially worse it would become if we had even more objects.

### Back to mutable objects

The solution was to simply modify the initial reduce object on each iteration, which some might argue it’s easier to understand the code now, despite looking slightly worse.

```javascript
const uidMap = objects.reduce((acc, obj) => {
  acc[obj.uid] = obj.resourceVersion;
  return acc;
}, {});
```

The results? From ~3 seconds to 10ms. That’s a **300x improvement**!

I need to rewire my brain to stop using spread operator unless it’s really necessary — oh well, I guess this applies to so many other things right?

Some habits die hard, I suspect this will be one of those.
