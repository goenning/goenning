---
layout: post
title: Measuring the performance of a website from multiple locations on a budget
comments: true
lang: en
tags: [docker, web, performance, azure]
description: Measuring the performance of a website from multiple locations around the world is crucial with the current global scale of the internet. In most cases, your visitors are not only based in your home country but from all other countries too. From Canada to Australia, from Chile to Russia, your website is being visited by more people than you think. There are a number of paid services that allow you to constantly monitor your website performance from multiple locations around the world. But maybe you're a geek and you want to do it yourself?
---

## Global metrics for websites with global visitors 

Measuring the performance of a website from multiple locations around the world is crucial with the current global scale of the internet. In most cases, your visitors are not only based in your home country but from all other countries too. From Canada to Australia, from Chile to Russia, your website is being visited by more people than you think.

It's easy to forget this fact and simply measure the performance of a website from the local machine, where it's usually close to the website Data Center. Not only that, but this machine is probably a beast too, it can open multiple tabs of Google Chrome and load any page with an insane amount of JavaScript in half a second.

But in reality, your visitors are using a 10 years old laptop with Windows 7 and are connected to the internet via a 3G network connection. Your Data Center is in San Francisco and these visitors are from The Philippines. Even your smartphone has 10x higher speed and lower latency than theirs.

There are a number of paid services that allow you to constantly monitor your website performance from multiple locations around the world.

But maybe you're a geek and you want to do it yourself?

On this post, I want to show you how to collect website performance from multiple locations using open source tools on a low monthly cost.

## webpage-timing

[goenning/webpage-timing](https://github.com/goenning/webpage-timing) is Node.js application that uses Headless Chrome to collect performance metrics from a web page.

This application is also available as a Docker image, so you can run it like this:

```sh
$ docker run goenning/webpage-timing
```

Go on, give it a try and execute this on your Terminal.

You should get a response similar to this:

```json
{ "start": "2019-02-10T16:32:25.655Z",
  "start_ts": 1549816345655,
  "duration": 430,
  "origin": "iMac.local",
  "request_url": "https://example.org",
  "metrics":
   { "Timestamp": 300473.456095,
     "Documents": 2,
     "Frames": 1,
     "JSEventListeners": 0,
     "Nodes": 39,
     "LayoutCount": 1,
     "RecalcStyleCount": 2,
     "LayoutDuration": 0.053402,
     "RecalcStyleDuration": 0.000584,
     "ScriptDuration": 0.000015,
     "TaskDuration": 0.068091,
     "JSHeapUsedSize": 2108152,
     "JSHeapTotalSize": 3936256 },
  "entries":
   [ { "name": "https://example.org/",
       "entryType": "navigation",
       "startTime": 0,
       "duration": 427.6149999932386,
       "initiatorType": "navigation",
       "nextHopProtocol": "h2",
       "workerStart": 0,
       "redirectStart": 0,
       "redirectEnd": 0,
       "fetchStart": 0.2899999963119626,
       "domainLookupStart": 7.634999987203628,
       "domainLookupEnd": 8.984999964013696,
       "connectStart": 8.984999964013696,
       "connectEnd": 313.34999995306134,
       "secureConnectionStart": 0,
       "requestStart": 313.6149999918416,
       "responseStart": 414.7049999446608,
       "responseEnd": 415.8099999767728,
       "transferSize": 800,
       "encodedBodySize": 606,
       "decodedBodySize": 1270,
       "serverTiming": [],
       "unloadEventStart": 0,
       "unloadEventEnd": 0,
       "domInteractive": 426.6899999929592,
       "domContentLoadedEventStart": 426.7049999907613,
       "domContentLoadedEventEnd": 426.7049999907613,
       "domComplete": 427.6049999753013,
       "loadEventStart": 427.6149999932386,
       "loadEventEnd": 427.6149999932386,
       "type": "navigate",
       "redirectCount": 0 },
     { "name": "first-paint",
       "entryType": "paint",
       "startTime": 491.65999999968335,
       "duration": 0 },
     { "name": "first-contentful-paint",
       "entryType": "paint",
       "startTime": 491.68499995721504,
       "duration": 0 } ] }
```

This is a `Timing` object, it contains some information collected from the Headless Chrome that executed inside the container. In the example above, it took **430ms** to load **https://example.org**. If this page had any CSS/JavaScript/Image files, it'd also download it and the duration would have been higher.

You can also specify some custom arguments, which allows you to collect metrics from any page. 

```sh
$ docker run -e REQUEST_URL=https://github.com/docker goenning/webpage-timing
```

You'll also notice that the `entries` array has many more items now, which includes all the CSS/JavaScript/Image files the browser had to download.

Another useful parameter is `MONGO_URL`, which allows you to store the `Timing` object into a MongoDB collection instead of printing it to stdout. It is useful when you want to keep a history of execution over time and perform further analysis.

```sh
# Replace the connection string below with your own
$ docker run -e MONGO_URL=mongodb://user:pass@your-server:port/db -e REQUEST_URL=https://github.com/docker goenning/webpage-timing
```

## Running it on a global infrastructure

We've seen so far that we can collect metrics from a web page using a local Docker container and store it in MongoDB.

To go one step further we'll use machines on the Cloud to run this container from multiple regions. You could spin up multiple Virtual Machines on the cloud provider of your preference and schedule this script on cron. But that's not very cost effective and you'd have to carry the burden of maintaining dozens of virtual machines.

But there's a better way 😀

Azure has a service called [Azure Container Instances](https://azure.microsoft.com/en-us/services/container-instances/). It allows you to run a Docker container on the cloud without having to worry about the infrastructure behind it. The best of all? You only pay per execution time. If you start a container that runs for 5 seconds, you'll pay $0.000080. On this post, I'll show you how to perform this operation on Azure, but If prefer AWS, search for `AWS Fargate`, it's a similar service, so you apply the same idea presented here.

What we're going to do is create dozens of Azure Container Instance on each of its 17 regions and configure it to execute `goenning/webpage-timing` with our custom parameters. We'll also need to store the data from all locations to query it later. In this example, I'll be using [MongoDB Atlas](https://www.mongodb.com/cloud/atlas) because it has a free tier and can also be hosted on Azure.

Assuming that you have an Azure Account and the [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/install-azure-cli) is installed, run the following commands.

```sh
$ az login
$ az account set --subscription "<YOUR_SUBSCRIPTION NAME>" # you can skip this if your account has only one subscription
```

Create a file `aci.sh` with the following content:

```sh
# The name of the resource group to be used on Azure
resource_group="webtiming-rg"

# The list of locations from where the test will be executed
locations=( "westus" "eastus" "westeurope" "westus2" "northeurope" "southeastasia" "eastus2" "centralus" "australiaeast" "uksouth" "southcentralus" "centralindia" "southindia" "northcentralus" "eastasia" "canadacentral" "japaneast" )

# The URL of the webpage we want to test
request_url="https://github.com/docker"

# The connection string to a MongoDB instance
mongo_url="mongodb://user:pass@your-server:port/db"

if [ $1 == "init" ]
then
  az group create --name $resource_group --location eastus
fi

if [ $1 == "run" ]
then
  for loc in "${locations[@]}"
  do
    for i in {1..20}
    do
      status=$(az container show -n $loc-wt-$i --resource-group webtiming-rg --query containers[0].instanceView.currentState.state 2>/dev/null)
      if [ $? -eq 0 ]
      then
        az container start -g $resource_group --name "$loc-wt-$i" &
        echo "$loc-wt-$i has started..."
      else
        az container create -g $resource_group --name "$loc-wt-$i" --image goenning/webpage-timing --cpu 1 --memory 1 --location $loc --restart-policy Never --no-wait --ip-address Private --environment-variables ORIGIN=$loc REQUEST_URL=$request_url --secure-environment-variables MONGO_URL=$mongo_url
        echo "$loc-wt-$i has been created..."
      fi
    done
  done
fi

if [ $1 == "clean" ]
then
  az group delete --name $resource_group --yes
fi
```

The first lines are the parameters, configured it based on given comments and execute:

```sh
$ ./aci.sh init
```

This first step will simply create a Resource Group based on the configured name. You can skip this step if you prefer to do it manually through Azure Portal.

```sh
$ ./aci.sh run
```

This is the most important part of the script. It'll basically loop through each configured location and create 20 Azure Container Instance on that location.

By the end of the execution, you should have something similar to this on your Azure Portal.

![](/public/images/2019/02/aci-webpage-timing.png)

Notice that there are 340 container instances, 20 on each region. Some of them have already finished processing, while others are still in progress. This process can take a few extra seconds as Azure needs to pull the images from Docker Hub Registry first.

A few seconds later all 340 instances should be on "Succeeded" state. By the end of this process, these instances will remain on Azure until you remove it. You can do so by executing `./aci.sh clean`, which removes the Resource Group and all of its container instances.

But if you plan to periodically execute this, you can keep the resources on Azure and simply execute `./aci.sh run` again. The script is smart enough to restart the container if it already exists on Azure. You can repeat this process as many times as you want.

## The results

We should now have 340 documents on our MongoDB database, so we can now look at the data and perform some analysis. We could hook up any BI tool to this MongoDB instance, extract the data and plot some charts.

But there is also [MongoDB Charts](https://www.mongodb.com/products/charts), which is a data visualization tool to create visual representations of our MongoDB Data. At the time of this writing, this service is on beta and free to use, so I decided to give it a try. This is what I got from my execution.

![](/public/images/2019/02/mongodb-charts.png)

As we can see, we have collected 337 timings (3 instances have failed...) and the global average is 2385ms. The chart shows that loading `https://github.com/docker` on US regions is 2x faster than on Asian Data Centers, which is expected since GitHub is hosted in the US.

You could go one step further and actually analyze the `entries` array to find out which HTTP resources took longer to load.

## Where to go from here?

If you liked this and want to take it to the next level, here are some ideas:

1. Schedule this script to execute every X hours
2. Reduce the Docker Image size. The smaller the image is, the faster it'll be executed on the Cloud, which means less 💸
3. Fork the project and enhance it with extra timing information you need
4. Use puppeteer to emulate a slower network and CPU
5. Change the script to be a multi-step process. If you have an e-commerce and you want to measure how long does it take for a user to find a product and buy it. You can use this script as a starting point and include the extra steps of this process
6. Change the script to retry when the container fails
7. There is an option to deploy multiple containers in a single Azure Container Instance resource by using YAML file. I haven't tried it yet, but that should give better performance and scale, but the script would become more complicated, hence why I decided to keep this example simple and create one resource per container
8. After implementing #7, go wild and deploy hundreds of containers per region 😁, just keep in mind that it'll also increase your 💸

## That's all! 🎉

Please leave a comment, suggestion or feedback!