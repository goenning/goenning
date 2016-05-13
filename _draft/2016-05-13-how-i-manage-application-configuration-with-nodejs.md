---
layout: post
title: How I manage application configuration with Node.js
comments: true
tags: [nodejs, environment, configuration]
---

Every application needs configuration settings and most programming languages/frameworks ecosystem has some kind of default support and convention for it.

- Most Java applications rely on `.properties` files;
- .Net has it's default standard `app.config` and `web.config` files;
- Ruby on Rails load settings from `config/application.rb`, `Initializers` and `After-initializers`;

Node.js does not have a standard way of defining these settings, but there are a lot of options available.

What I'll show here is how I do it in my projects.

I wanted something that could be easy to use, easily readable, safe, environment-specific and no package dependency.

I've choose to have a single file for each environment, which are `development`, `test`, `staging` and `production`.

Dependencies:
- [lodash](https://www.npmjs.com/package/lodash) package. You're probably already using it, or will do it someday. It's very useful and commonly used on most JavaScript applications.



Talk about:
- NODE_ENV [production, staging, test, development]
- The ease of reading a json configuration file
- The ease of deployment, not worrying about forgeting to update some configs
- The ease of coding, just import the configuration and let code decide which file to load
- Allows multiple environment, like development, testing, staging, production, etc
