---
layout: post
title: How to achieve 100% code coverage when using TypeScript
comments: true
tags: [typescript, istambul, code-coverage]
---

An implication of using transpile-to-javascript languages is that the compiler may emit some code that you did no expect. TypeScript does it for what he calls helper functions. These functions are emitted on each of the compiled files that need them. The same function, over and over again.

When your goal is to have 100% code-coverage, you `will` need to test these functions as well, even if you didn't code. Because, in fact, your SUT (System Under Test) is your JavaScript transpiled code, not your TypeScript source code. But how do you test every single branch of these functions?

You sure can, but you'll need to code a lot more than usual. That probably means you'll end up with some useless tests just to get to 100% coverage. That's not what we want, correct?

## Example of some helpers functions

When using the awesome [async/await feature](http://blogs.msdn.com/b/typescript/archive/2015/11/03/what-about-async-await.aspx) of TypeScript the compiler will emit a function named `__awaiter`. The function currently looks like this.

```javascript
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, Promise, generator) {
    return new Promise(function (resolve, reject) {
        generator = generator.call(thisArg, _arguments);
        function cast(value) { return value instanceof Promise && value.constructor === Promise ? value : new Promise(function (resolve) { resolve(value); }); }
        function onfulfill(value) { try { step("next", value); } catch (e) { reject(e); } }
        function onreject(value) { try { step("throw", value); } catch (e) { reject(e); } }
        function step(verb, value) {
            var result = generator[verb](value);
            result.done ? resolve(result.value) : cast(result.value).then(onfulfill, onreject);
        }
        step("next", void 0);
    });
};
```

Another example is when you are working with TypeScript class inheritance and your target is ES5. Because ECMAScript 5 does not support classes, the compiler will emit the helper function `__extends` the same way it does with async/await.

```javascript
var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
```

There are also others helpers functions like  `__decorate`, `__param`, `__metadata`, etc.

## The solution

If you are about to get 100% code coverage and all you need is to cover the many helper functions your JavaScript has, here is the solution that worked for me.

The first step is to tell TypeScript to stop emitting helper functions to the compiled files. You can do so by setting the property `compilerOptions.noEmitHelpers` to `true` on your `tsconfig.json` file.

If you try to run your application after this change, it'll not work, because it will not have the definition of the helper functions they need to work properly.

So the next step it to create the function by yourself. Find the entry point of you package/application and add the following.

```javascript
global["__awaiter"] = function(thisArg, _arguments, Promise, generator) {
  return new Promise(function(resolve, reject) {
    generator = generator.call(thisArg, _arguments);
    function cast(value) { return value instanceof Promise && value.constructor === Promise ? value : new Promise(function(resolve) { resolve(value); }); }
    function onfulfill(value) { try { step("next", value); } catch (e) { reject(e); } }
    function onreject(value) { try { step("throw", value); } catch (e) { reject(e); } }
    function step(verb, value) {
      let result = generator[verb](value);
      result.done ? resolve(result.value) : cast(result.value).then(onfulfill, onreject);
    }
    step("next", void 0);
  });
};
```

> You may need to add [nodejs](https://github.com/DefinitelyTyped/DefinitelyTyped/blob/master/node/node.d.ts) typings using `tsd` or `typings` if still don't have it

Do the same for each helper you may need.

Now your App is ready to launch. It should be working again, your tests should be passing and your code coverage should have increased.

## Still not 100%?

There is also a particular problem with `__awaiter` specifically. There are two catch blocks that I did not find a way to test it.

![](/public/images/reject-awaiter.png)

So, if you really want to get 100% coverage, you will need to ignore this function from coverage. Ignoring this function from code coverage could be seen as a hack, but I don't think about it that way. This function was created and tested by TypeScript developer

I'm using [Istanbul](https://www.npmjs.com/package/istanbul) for my project, so in my case I had to add a special comment on top of the function definition. And oh, don't forget to set `compilerOptions.removeComments` to `false` on your `tsconfig.json` file, that will prevent TypeScript from removing this special comment.

```javascript
/* istanbul ignore next */
global["__awaiter"] = function(thisArg, _arguments, Promise, generator) {
  ...
}
```

For a working example you can look at the source code of [goenning/guard-me](https://github.com/goenning/guard-me). This is a project I'm working on and is using this technique.

Cheers!