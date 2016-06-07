---
layout: post
title: How I manage application configuration with Node.js
comments: true
tags: [node.js, environment, configuration]
---

Every application needs configuration settings and most programming languages/frameworks ecosystem has some kind of default support and convention for it.

- Most Java applications rely on `.properties` files;
- .Net has it's standard `app.config` and `web.config` files;
- Ruby on Rails load settings from `config/application.rb`, `Initializers` and `After-initializers`;

Node.js does not have a standard way of managing these settings, but there are a lot of options available.

What I'll show here is how I do it in my projects. I wanted something that could be easy to use, easily readable, flexible, environment-specific and with minimal package dependency.

## The solution

Dependencies: [lodash](https://www.npmjs.com/package/lodash) & [dotenv](https://www.npmjs.com/package/dotenv)

The folder structure will look like this:

![](/public/images/config-folder-nodejs.png)

`default.js` is a file in which **all** configuration settings are exported as a JSON object. Each key may have a hard-coded value or a reference to an environment variable.

~~~javascript
module.exports = {
  cookie: {
    secret: process.env.COOKIE_SECRET_KEY,
  },
  session: {
    secure: true
  },
  db: {
    provider: "pg",
    connection: process.env.DATABASE_URL
  },
  redis: {
    url: "redis://some-redis-server:6379"
  }
  log: "debug"
};
~~~

For each environment there is a JavaScript file that also exports a JSON object, but it's not as complete as the file above, it only contains those settings that are specific to the environment. The most common environments are `development`, `test`, `staging` and `production`.

`<environment>.js` can be used to override any settings. In the following example, for the `production` environment we don't want to log debug messages, so we can actually override it to log only the important messages.

Redis URL is also set to use environment value, while the default value is a hard-coded string.

`production.js`

~~~javascript
module.exports = {
  log: "error",
  redis: {
    url: process.env.REDIS_URL
  }
};
~~~

All these files are stored in a folder named `config` with an `index.js` file that exports a merged JSON object of both `default.js` and `<current_environment>.js`. If `NODE_ENV` environment variable is undefined, `development` is assumed as default environment.

~~~javascript
var _ = require("lodash");
var defaults = require("./default.js");
var config = require("./" + (process.env.NODE_ENV || "development") + ".js");
module.exports = _.merge({}, defaults, config);
~~~

When there is a need to read any configuration settings, it's as easy as `require('path/to/config')`. There is no need to actually load the environment file directly, let the `index.js` do that dirty job.

~~~javascript
var config = require('./config');
...
app.use(require('cookie-parser')(config.cookie.secret));
...
app.use(session({
  saveUninitialized: true,
  resave: false,
  store: new RedisStore({
    url: config.redis.url
  }),
  secret: config.cookie.secret,
  cookie: { httpOnly: true, secure: config.session.secure }
}));
...
~~~

By doing this, I have a clean and flexible solution. I can rely on server's environment variables to store sensitive data and it it's also possible to set hard-coded values for each environment directly in each of the environment files, as well as a default value in `default.js`.

Environment values are usually **ONLY** set on staging and production servers. I don't like to mess with my OS environment values, so I opted to use [dotenv](https://www.npmjs.com/package/dotenv), which is a super tiny package that looks for a `.env` file inside your project root directory. If it's found, it sets current process environment according to file contents. If it's not found (in production, for example), it does nothing. So there is no harm in using it.

It's typical that `.env` should be only used for development and testing purpose, that means it should be add to `.gitignore`, as it's content can vary by developer. If you ever commit and send it to production, it's very likely that you'll run into trouble, because it'll override all serves environment values.

**Note:** When using `.env` file, don't ever forget to require it as soon as your application starts.

~~~javascript
require('dotenv').config({ silent: true });
~~~

## What you think about it?

Do you like? Does it fit into your application?

Leave your comments about this solution and share with me what do you use to store your application settings
