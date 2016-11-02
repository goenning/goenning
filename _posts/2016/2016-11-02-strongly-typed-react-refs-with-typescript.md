---
layout: post
title: Strongly Typed React Refs with TypeScript
comments: true
lang: en
tags: [typescript, react]
ref: strongly-typed-react-refs-with-typescript
---

If you've been working with React for some time you probably figured out that [`refs`](https://facebook.github.io/react/docs/refs-and-the-dom.html) are very easy to start with, 
but rather [inflexible](http://stackoverflow.com/questions/29503213/use-state-or-refs-in-react-js-form-components) after some point.

But anyway, `refs` are available and they work. So why not use them?

When working with TypeScript it's usually a common practive to have everything as strongly typed as possible.

Unfortunately `refs` is typed as `[key: string]: ReactInstance`, which force us to type `this.refs["myInput]`. This is definitely not what we want.

### Using callback references

`ref` as a callback is the recommended approach on how to use it nowdays, and the solution with TypeScript is pretty simple.

```typescript
import * as React from "react";

export class CustomTextInput extends React.Component<{}, {}> {
  private textInput: HTMLInputElement;

  constructor() {
    super();
    this.focus = this.focus.bind(this);
  }

  public focus() {
    this.textInput.focus();
  }

  public render() {
    return <div>
              <input
                type="text"
                ref={(ref) => this.textInput = ref}
              />
              <input
                type="button"
                value="Focus the text input"
                onClick={this.focus}
              />
           </div>;
  }
}


export class AutoFocusTextInput extends React.Component<{}, {}> {
  private input: CustomTextInput;

  constructor() {
    super();
  }

  protected componentDidMount() {
    this.input.focus();
  }

  public render() {
    return <div>
              <CustomTextInput
                ref={(ref) => this.input = ref}
              />
           </div>;
  }
}

```

This example is **exactly** the same as in the [official ref docs](https://facebook.github.io/react/docs/refs-and-the-dom.html). 
The only single difference is that we have add a typed property to our class `private textInput: HTMLInputElement;`. 
With this is now possible to safely type `this.input.value`, `this.input.focus()`, `this.input.maxLength` and so on, without compiler warnings.

### Using string references

[The string refs are eventually going to be deprecated](https://github.com/facebook/react/issues/6250). 
But in case you're still using it and don't want to migrate to callback references, it's also pretty simple to add typings to it.

```typescript
import * as React from "react";

export class CustomTextInput extends React.Component<{}, {}> {
  public refs: {
    textInput: HTMLInputElement;
  };

  constructor() {
    super();
    this.focus = this.focus.bind(this);
  }

  public focus() {
    this.refs.textInput.focus();
  }

  public render() {
    return <div>
              <input
                type="text"
                ref="textInput"
              />
              <input
                type="button"
                value="Focus the text input"
                onClick={this.focus}
              />
           </div>;
  }
}
```

We're simply overriding the `refs` property to be strongly typed. Yeah, that's all. 

The difference between this example and the above is that this one is using the standard React `refs` property while the other is definining a new one.

Whatever you prefer, just don't forget to "type" your React components as much as possible.