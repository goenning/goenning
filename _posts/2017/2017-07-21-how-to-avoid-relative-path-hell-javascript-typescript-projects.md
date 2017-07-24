---
layout: post
title: How to avoid relative path hell in JavaScript / TypeScript projects
comments: true
lang: en
tags: [typescript, javascript, module resolution]
cover: /public/images/2017/07/relative-path-hell.jpg
abstract: Relative path hell is a common pain point on large JavaScript/TypeScript projects. Learn how Webpack can help you avoid this problem.
---

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Thanks to <a href="https://twitter.com/hashtag/typescript?src=hash">#typescript</a> and <a href="https://twitter.com/hashtag/webpack?src=hash">#webpack</a> I can now freely move my components without having to deal with relative path hell :) <a href="https://t.co/q257QPoylI">pic.twitter.com/q257QPoylI</a></p>&mdash; Guilherme Oenning (@goenning) <a href="https://twitter.com/goenning/status/880884293500850176">June 30, 2017</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

Those who ever worked on a JavaScript project are familiar with relative path hell on module resolution. 

Whenever a local module A depends on another local module B, it has to be imported using relative path, like these two examples.

```typescript
import { DefaultRenderer } from '../../../rendereders'
import { PayPalClient } from '../../services/PayPal'
```

This approach brings some issues to the table whenever your codebase starts to grow. There's a need to know where both the current and target module are in the folder structure. This is a big hurdle to refactoring and readability. 

Where is `../../../rendereders` located? Can you easily find it? One or two folders above might be ok, but more than that you get a ticket to hell.

On regards to refactoring, every time **ANY module** is moved up or down the tree, there'll be a need to fix the relative path of both consumers and imported modules. Not a fun thing to do, right?

## Webpack to the rescue

Webpak can avoid this issue by creating an alias for a specific directory, so that instead of using the relative path, it would be possible to use only the alias or a combination of alias and path.

Previous example can be rewritten to.

```typescript
import { DefaultRenderer } from '@app/renderers'
import { PayPalClient } from '@app/services/PayPal'
```

Cool, isn't it? You don't need to care where your current module is, just where to find target module starting from root path. This can be easily achieved by adding a `resolve.alias` to your `webpack.config.js`.

```javascript
resolve: {
  alias: {
    '@app': path.resolve(__dirname, 'src/')
  }
}
```

In this example everything that starts with `@app` will be loaded from `src/`. I like to call it `@app` as it becomes easy to know whenever you're importing something from your app rather than a NPM module.

The benefit in this case is that no matter where you move the file that imports these modules, there's no need to change the path.

It's worth mentioning that this also works for scss files, but you'll need to prefix it with **~**.

```scss
@import '~@app/styles/variables.scss';
```

## Using TypeScript? No worries, we've got your covered

In case you're using TypeScript and try do this, you'll soon see errors on the compiler complaining that the modules could not be found. Which makes sense, because TypeScript compiler doesn't know where to find these modules. 

There's a particular compiler setting that can change module resolution inside TypeScript so that it'd find these modules the same way that Webpack does.

```json
{
    "compilerOptions": {
        "baseUrl": "./src",
        "paths": {
            "@app/*": [
                "*"
            ]
        }
    }
}
```

That's all you'll need. The compiler will now know where to find type definition of any module that starts with `@app`.

*Please note that these settings are NOT mutually exclusive, if you're using TypeScript you'll need both Webpack AND TypeScript settings.*

Hope this can help you make your code better.

Happy Coding!