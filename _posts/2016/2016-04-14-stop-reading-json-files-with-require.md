---
layout: post
title: Stop reading JSON files with require
lang: en
tags: [javascript, node.js]
ref: stop-reading-json-files-with-require
---

There you are, coding at 11:00 PM, trying to get your unit tests back to a green state and no matter how hard you try, you **just can't find why**. Not even a dozen of console.log can help you.

Well, that was me last night.

![](/public/images/hate-programming.jpg)

At this stage I should probably make a tattoo **stop reading JSON files with node's require function** or even put it up somewhere on the wall. Ok, that's too much, but after losing 1 hour troubleshooting it deserves at least a blog post.

### Reading JSON files with Node.js

JavaScript is beautiful when it comes to JSON. There is no need for a third-party `JSON Parser libraries`, (such as [Json.NET](http://www.newtonsoft.com/json) or [Gson](https://github.com/google/gson)) and it's pretty straightforward to parse a JSON file into a variable, just use `require()`, like this:

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

Easy and readable, right?

But that function has an important *side effect* that can be both `good` and `bad`, that will depend on the context of your application.

In my case, I was using it in a unit test and it was causing more harm than good.

Ok, let's get straight to the point.

`require()` will always cache the content of the loaded module (our JSON file, in this case). The next time `require()` is called for the same module, it will restore it from the cache instead of reading it again. That's awesome! But...

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

The solution is quite easy, just use the built-in `fs` module when reading JSON files. I'm now using the following helper function.

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
