---
layout: post
title: Business and technical overview of tudoTRI
lang: en
tags: [triathlon]
---

I've always wanted to build an useful website that could help others somehow. I decided to do it by combining two of my life passions: computer programming and triathlon.

The idea of this project came from a day I was looking for a particular cycling component for my bike. I had **waste more than 2 hours** *googling* it and visiting dozens of online shops. The search was really helpful, because the price was very different between each site, it was ranging from R$ 200,00 to R$ 400,00. If I had bought from the first site I found, I'd probably waste more than necessary, resulting in a **bad purchase** for me.

From that day on, I new that I could do something to make this experience better, for me all others athletes. It would have place in the market and it was just a matter of time until I get the first version up and running.

After a few weeks the project came to public.

![](/public/images/tudotri-logo.png)

[**tudoTRI**](http://tudotri.com.br) is a Brazilian website that aggregates products from multiple local e-commences into a single place. By doing so, we are able to show our visitors what are the best offers for the products they are looking for.

## Technical overview

For those who got here from my [live streaming](https://www.livecoding.tv/goenning/) on livecoding.tv, here is a technical overview of how it was built and how I run it.

![](/public/images/tudotri-workspace.jpg)

#### Project structure

- **Core:** it's like a shared module, containing useful functions and classes that are used by the others projects;
- **API:** the heart of the whole application, where business rules are coded and acting as a single point of access to the database;
- **Admin:** the web application used by me to do administrative tasks like editing products, stores, users, configurations, etc.;
- **Web:** the public web site (http://tudotri.com.br);
- **Bot:** the crawler/scraper. His job is to visit the configured online shops to get the latests updates on prices and products;

#### Development Stack:
- [Node.js](https://nodejs.org/)
- [TypeScript](https://www.typescriptlang.org/)
- [Express](http://expressjs.com/)
- [Postgres](http://www.postgresql.org/)
- [AngularJS](https://angularjs.org/) & [jQuery](https://jquery.com/) & [Vanilla JS](http://vanilla-js.com/)

#### Development Tools:
- Mac OSX
- [Atom](https://atom.io/) + [atom-typescript](https://atom.io/packages/atom-typescript)
- [pgAdmin](http://www.pgadmin.org/) (very buggy, does anyone have a better alternative?)

#### Production Environment:
- [Ubuntu](http://www.ubuntu.com/) on [DigitalOcean](https://www.digitalocean.com/)
- [PM2](http://pm2.keymetrics.io/) for node process management
- [NixStats](https://nixstats.com/) for server monitoring
- Cron (to start the bot periodically)

#### Misc.:
- The code is not open source, although you can see part of it while I'm [live coding](https://www.livecoding.tv/goenning/)
- Code is written in English, but database and website content is Brazilian Portuguese.
