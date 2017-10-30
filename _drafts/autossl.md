HTTPS is becoming a must nowadays. Not only it’s a security measure, but also because Google gives better rank to websites using a secure protocol over those using plain HTTP.

Days are gone that we can use the excuse that SSL certificates are expensive to buy and install on your servers.

## Let’s Encrypt 

Let’s Encrypt is SSL certificate issuer that provides a free and automated generation process. It is possible to issue a certificate in less than a second without any registration process or payment.

On this blogpost I will show you how to deploy a very basic Go web application on the Cloud, running on HTTPS without paying a penny for it.

## Requirements

If you wish to follow this demo and try it yourself, please make sure you comply with following requirements.

- You need the Go compiler;
- A Virtual Machine publicly available (Im using a Digital Ocean VM for this demo);
- A domain name and access to it’s DNS settings. You’ll not need this if your cloud provides a public shared name like: yourvn0001.yourcloud.net

## Autocert

Autocert is Go package that simplifies the communication and certificate fetch process with Let’s Encrypt. This is the only external dependency you will need.

First thing you got to do is install autocert. You can install it with the following command.

Go get ...

## The code

Create your new project and paste this on your main file.

...

Let me explain what’s happening here.

We basically import the autocert dependency and create an autocert.Manager struct that is responsible for communicating with Let’s Encrypt.

...
Cache
GetCeetificate
Enpty Strings


## Let’s run it!

You can run it like any other Go web app, but if you do it on your local machine, it fail. The reason being that Let’s Encrypt requires that your app is publicly available through a know DNS name. When you run it locally, Let’s Encrypt cannot ping back your domain for verification purposes and it’ll fail miserably.

At this point I expect you to have your VM and your domain access. 

1) First of all, create a new DNS A record using your VM public IP.
2) Compile your Go app with “blá-blá-blá”
3) Copy your binary to the server with “blá-blá-blá”
4) Log into your server with SSH and start your Go server by simply running the binary you have just copied.
5) Open your browser with the address of your DNS domain.

Boom!

You should see the SSL lock icon and the green check (depending on the browser you’ve used).

You might notice that it takes a while to load the first request. That’s because all the SSL certificate generation process is happening on the background. Consecutive request should be lightning fast as the certificate is already cached.

## Important notes and suggestions:

1) There’s a limits on how many certificates can be generated for the same domain. At the time of this writing, the limit is 20 certificates per week. It might look a lot, but if you don’t manage your severs properly you might easily reach this limit.

2) It’s up to you to decided where and how you’re going to store the certificate. The temporary local storage works great when you have only a few servers. Once your cluster grows, this method might not suit you as each server will fetch a new certificate. Remember the limit I explained above.

3) The certificates are generated for 90 days. Thankfully autocert handles renewal automatically so you don’t need to do nothing. But it’s always a good practice to keep an eye on it when it’s close to expire date.

4) Try to use it on a multi-tenant application where each tenant has its own subdomain and you’ll fail. I learned the hard way. The limit is by domain name, excluding sub domain, so you can only generate certificates for 20 tenants a week.

5) As you cannot use autocert locally, you’ll need to build your app in such a way that you can choose between running on HTTP or HTTPS, so that you can easily use HTTP during development.

If you have any question, feedback or suggestion, please feel free to drop me a comment. I’m happy to help if I can.

Thanks!