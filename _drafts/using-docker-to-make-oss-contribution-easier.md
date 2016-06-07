---
layout: post
title: Using Docker to make OSS contribution easier
comments: true
tags: [node.js, Docker, open source]
---

When surfing through GitHub it's not hard to find projects that have a complex 'how to contribute' guide that helps newcomers to set up a bare minimum development environment.

But we also happens to find projects that are as simple as 'git clone', 'npm install' and 'npm test'. Ok you're ready, grab and issue and start coding!

Sure there projects that are way more complex than an utility library, for example, because it's becoming more and more common to find projects that heavily depends on Redis, ElasticSearch, SQL Databases and so on. Having to install all these services locally doesn't make sense at all, it will eventually slow down your machine while your note coding and you'll end up with a ton of services running that are most of the idle. Knex, a ... tool for node.js, suggests that you install MySQL and Postgres locally with a default username, password and database name so that you can their integrations tests.
(https://github.com/tgriesser/knex/blob/master/CONTRIBUTING.md)

But it turns out that there is a quick and easy solution for that...

Docker, the almighty savior

It's really easy and fast to launch a third-party service like Redis or ElasticSearch. In single docker run command you can have a Redis container up and running, without touching your local environment. You can then use it to run your integrations tests.

Most of the time, the standard Docker images for most of the services are enough, but if you find yourself in a situation that needs some sort of customization, you can always roll out your own Dockerfile and publish it to the Hub. That way anyone interested in contributing to your open source project can actually simulate the same environment you use for testing.

Want to know something cool? Travis-CI and AppVoyer(??) also support Docker out of the box, so you can make your build service run the same environment as your dev machine.

Let's dig into an example using miniprofiler-pg, a library that intercept all postgres commands that are issued by the application and provide helpful information to MiniProfiler. It's a small package, so I ended up with a small test suite, but it's good example on how to setup everything.

Git clone the project, npm install it, run start-services (wait for Postgres image to download if you don't have it already) and then issue a npm test.

If everything went well, you'll end up with a success message from Mocha.

See that in only 4 commands you got yourself with a container running Postgres that is used by miniprofiler-pg to run the tests. You didn't have to install Postgres from scratch, create a new user, create and a database and so on.

To use docker in your OSS project is really easy. First decide which images you need

 and check the package.json file. There's a script called "start-services", which contains a docker run command that is used to start our dependency service.

Docker-ip