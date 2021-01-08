---
layout: post
title: Using Azure Application Insights to monitor usage of a Next.js application
lang: en
tags: [TO BE DEFINED]
description: TO BE DEFINED
---

[Azure Application Insights](https://docs.microsoft.com/en-us/azure/azure-monitor/app/app-insights-overview) (aka: AI) is an Application Performance Management for web applications. It could be compared to Google Analytics as it's used to track page views and overall application usage, but the focus of AI is on the performance and exceptions monitoring.

In a tradicional server-side rendered web application, using AI is as simple as dropping a script tag onto the HTML response. This script will then track all the page views, exceptions and dependencies calls for us without having to write any code. But client-side rendered application is a completely different story, in which a few extra steps are needed for the AI to work.

# Using AI on a Next.js application

On a Next.js application there's the concept of `pages`, but behind the scenes these are basically React Components. These components are rendered on the server during initial load, but on subsequent page views, the render process happens on the client side. Because of this, Next.js applications requires some extra code for AI to work.

`next-applicationinsights` is a NPM package that exposes a [Higher-Order Components](https://reactjs.org/docs/higher-order-components.html) that decorates the whole Next.js application with AI tracking.

To install it, run the following command:

```sh
npm install next-applicationinsights
```

If you don't have a custom App component yet, you'll need to create one. The custom App is always named  `pages/_app.js`. The custom App must then be wrapped on a call to `withApplicationInsights`, so the code looks like this:

```javascript
import App, { Container } from 'next/app'
import { withApplicationInsights } from 'next-applicationinsights';
 
class MyApp extends App {
  render() {
    const { Component, pageProps } = this.props
 
    return (
      <Container>
        <Component {...pageProps} />
      </Container>
    )
  }
}
 
export default withApplicationInsights({ 
  instrumentationKey: 'YOUR_KEY_GOES_HERE',
  isEnabled: true
})(MyApp)
```

You must then insert your own `instrumentationKey` that can be found on the Azure Portal under the AI instance properties.

**Hint:** To disable AI tracking during development, use this configuration `isEnabled: process.env.NODE_ENV === 'production'`. This will basically enable AI **only** on Production builds.

This package uses [@microsoft/applicationinsights-web](https://www.npmjs.com/package/@microsoft/applicationinsights-web), so all the configuration options from this package are supported by `withApplicationInsights`.

# The result 