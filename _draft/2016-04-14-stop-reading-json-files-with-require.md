And there you are, coding at 11:00 PM, trying to get your unit testing back to the green field.
You try as hard as you can, you throw `console.log` for every function call and you `just can't find **WHY**!`.
Why isn't it working anymore? Why?!

That was me last night.
And this is me now.

:)

It reminds this image https://s-media-cache-ak0.pinimg.com/736x/d4/f2/00/d4f20041254a0727ddce7cb81be9e68c.jpg

After yesterday, I should probably tattoo **stop reading JSON files with node's require function.** or even put it somewhere on the wall. But after 2 hours of troubleshooting, this reserves at least a blog post.

### Reading JSON files with Node.js

JavaScript/Node.JS is specially beautiful when it comes to working with JSON. You don't need those nasty `JSON Parser libraries` (like [Json.NET](http://www.newtonsoft.com/json) and [Gson](https://github.com/google/gson)), that converts strings into objects — in which, of course, you need to create classes, even if you're going to use it once.

I've always used `require()` to read a json files into a variable, like this:

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
console.log(settings.tags) // ['javascript', 'nodejs']
~~~

Easy and clean, right?

But it has a *side effect* that can be both `good` or `bad`, that will depends on the context of your application.

In my case, I was using it in a unit test fixture and it was causing more harm than good. Ok, lets get straight to the point.

`require(...)` will always cache the content of the loaded module (or file, in our case). The next time you `require(...)` it again, it will restore if from the cache instead of reading it again. That's awesome! But...

In my case I was reading the file once for each test case. My first test case had changed a value from the JSON, and because it's content was cached by `require(...)`, the changed value was loaded into the second test case, in which I was expecting it to be the original value.

For those who like to see the code (everyone, I suppose), here is a practical hypothetical example.

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

There is also another problem with this approach. `require(...)` is synchronous, and as a good node.js developer, you know that blocking I/O is dangerous.

The solution is easy, stick with `fs` module when reading JSON files. I'm now using the following helper function.

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

Not cached, non-blocking IO and code is still very readable :-)

Cheers!
