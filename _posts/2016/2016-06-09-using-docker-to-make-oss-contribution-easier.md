---
layout: post
title: Using Docker to make OSS contribution easier
lang: en
tags: [node.js, Docker, open source]
ref: using-docker-to-make-oss-contribution-easier
---

While surfing through GitHub it's not hard to find projects that has a complex `how to contribute` guide that aims to help newcomers to set up a bare minimum development environment.

These projects are way more complex than an utility library, for example, because it's becoming more and more common to find projects that depends on Redis, ElasticSearch, SQL Databases and so on. Knex, for example, has a [CONTRIBUTING.md](https://github.com/tgriesser/knex/blob/master/CONTRIBUTING.md) guide suggesting that you install MySQL and Postgres locally with a given username, password and database name so that you can run their integrations tests.

Having to install all these services locally doesn't make sense, it will eventually slow down your machine while your not coding and you'll end up with a ton of services that are idle most of the time.

Aside from that, we also happens to find projects that are as simple as `git clone`, `npm install`, `npm test` and hooray! You're ready to grab an issue and start coding!

That's how it should work for every project, it encourages people to contribute to the project by having a simplified development environment.

It turns out that there is a quick and easy solution for that...

### Docker, the almighty savior

It's really easy (and fast!) to launch a third-party service like Redis, ElasticSearch or any DB. In a single `docker run` command you have a service container up and running, without touching anything in your local machine. You can then use it to run your project's integrations tests against it.

Most of the time, the standard Docker images are enough, but if you find yourself in a situation that needs some sort of customization, you can always roll out your own `Dockerfile` and publish it to your project's repository. Anyone interested in contributing to your open source project can actually simulate the same environment you used for testing.

Want to know something cool? [travis-ci](https://travis-ci.org/) supports Docker out of the box, so you can make your build service run the same tools you use in your development machine.

### Try it yourself

Let's dig into an example using [miniprofiler-pg](https://github.com/goenning/miniprofiler-pg), a library that monkey-patches all postgres commands that are issued by the application and provide helpful information to [miniprofiler](https://github.com/MiniProfiler/node). It's a small package, so I ended up with a small test suite, but it's good example on how to set up everything.

Before running the following commands, make sure Docker is installed you can run commands like `docker ps`.

```
git clone https://github.com/goenning/miniprofiler-pg.git
cd miniprofiler-pg
npm install
npm run start-services #this will start a new Postgres container using docker
npm test
```

If everything went well, you'll end up with a success result from Mocha tests.

See that in only 5 simple commands we got a container running Postgres that is used by miniprofiler-pg to run the tests. We didn't have to install Postgres, create any new user, database and so on.

You're now ready to work on the project. Easy, right?

### How does it work?

Looking at `package.json` file it's possible to see that `start-services` is just an alias to `docker run -d -e POSTGRES_USER=docker -e POSTGRES_PASSWORD=docker -p 5050:5432 postgres`.

This docker cmomand creates a new container based on [postgres](https://hub.docker.com/_/postgres/) image, create a new user named docker with password docker and a database named docker as well. That's all configurable, choose what best fits your needs. `-p` parameter binds the docker host port 5050 to the container port 5432.

To connect to the container's database is pretty straightforward, but there is a little gotcha when you're running [docker-machine](https://docs.docker.com/machine/). On MacOS and Windows hosts it's mandatory use to docker-machine because docker engine only runs on Linux systems. So what docker-machine does is provision a new VirtualBox VM to hosts the docker engine. In the end it's pretty much transparent, but your docker host you'll end up with a different IP address that your localhost.

It's possible to get the docker host's IP using `docker-machine ip`.

An easier way is to `npm install docker-ip --save-dev` and use it like this:

```javascript
var ip = require('docker-ip');
var connString = `postgres://docker:docker@${ip()}:5050/docker`;
```

`docker-ip` uses `docker-machine ip` and fallback to `localhost` when docker-machine is not available, like in Travis and most others Linux hosts.

To make it work with Travis it's necessary to add `docker` as a new service and start docker containers before running the test script. I had to add a `sleep 3` to my build recipe because the services was taking longer than expected to become available for use.

```yml
language: node_js
node_js:
  - "4"
  - "5"
  - "stable"

sudo: required

services:
  - docker

before_script:
  - npm run start-services
  - npm run lint
  - sleep 3

after_script:
  - npm run coverage
  - npm run check-coverage
  - npm run update-coveralls

notifications:
  email:
    on_success: never
    on_failure: change
```
