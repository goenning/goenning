---
layout: post
title: Stop reading JSON files with require
lang: en
tags: [javascript, node.js]
ref: stop-reading-json-files-with-require
---

And there you are, coding at 11:00 PM, trying to get your unit testing back to the green field. You try as hard as you can, you add `console.log` to every function and you `just can't find WHY!`. 

Why isn't it working anymore? Why?!

That was me last night.

![](/public/images/hate-programming.jpg)

After yesterday, I should probably tattoo **stop reading JSON files with node's require function** or even put it up somewhere on the wall. But after losing 1 hour while troubleshooting, this reserves at least a blog post.

### Reading JSON files with Node.js

JavaScript is beautiful when it comes to working with JSON. You don't need a third-party `JSON Parser libraries`, (such as  [Json.NET](http://www.newtonsoft.com/json) or [Gson](https://github.com/google/gson), that is used to parse a JSON string into an object — in which, of course, you need to create classes, even if you're going to use it only once.

I've always used `require()` to read json files into a variable, like this:

`settings.json`

~~~json
{
  "name": "My Application Name",
  "tags": [
    "nodejs",
    "javascript"
  ]
}
~~~

`index.js`

~~~javascript
var settings = require('./settings.json')
console.log(settings.tags) // ['nodejs', 'javascript']
~~~

Easy and clean, right?

But it has a *side effect* that can be both `good` and `bad`, that will depend on the context of your application.

In my case, I was using it in a unit test and it was causing more harm than good.

Ok, let's get straight to the point.

`require()` will always cache the content of the loaded module (or file, in this case). The next time `require()` is called again, it will restore it from the cache instead of reading it again. That's awesome! But...

I was reading the file once for each test case. The first test changed a value on that JSON, and because the content was cached by `require()`, the changed value was available on the second test case too. I was expecting it to be the original value, but it wasn't. The result? A broken test.

For those who like to see code, here is a practical hypothetical example.

~~~javascript
var expect = require('chai').expect
var fs = require('fs')

describe("Require", () => {
  it("should be able to change settings values", () => {
    var settings = require('./settings.json')
    settings.tags.push('v8')
    expect(settings.tags).to.be.deep.equal(["nodejs", "javascript","v8"])
  })

  it("should reload settings file", () => {
    var settings = require('./settings.json')
    expect(settings.tags).to.be.deep.equal(["nodejs", "javascript"])
  })
})
~~~

You might expect that both tests cases will pass. But they won't. The second will fail, because `settings.tags` now contains `"v8"`.

There is also another problem with this approach. `require()` is synchronous. As a good node.js developer, you know that blocking I/O is dangerous. It's ok to use it to load native or NPM modules, because we need it to be synchronous and cached, but JSON files are a separate story.

The solution is easy, stick with `fs` module when reading JSON files.

I'm now using the following helper function.

~~~javascript
var readJson = (path, cb) => {
  fs.readFile(require.resolve(path), (err, data) => {
    if (err)
      cb(err)
    else
      cb(null, JSON.parse(data))
  })
}
~~~

And our example would be rewritten to:

~~~javascript
describe("File System", () => {
  it("should be able to change settings values", (done) => {
    readJson('./settings.json', (err, settings) => {
      settings.tags.push('v8')
      expect(settings.tags).to.be.deep.equal(["nodejs", "javascript","v8"])
      done(err)
    })
  })

  it("should reload settings file", (done) => {
    readJson('./settings.json', (err, settings) => {
      expect(settings.tags).to.be.deep.equal(["nodejs", "javascript" ])
      done(err)
    })
  })
})
~~~

Non-cached, non-blocking IO, tests are passing and code is still very readable :-)

Cheers!
