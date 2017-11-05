HTTPS is becoming a must nowadays. Not only it’s a security measure, but also because Google gives better rank to websites using a secure protocol over those using plain HTTP.

Days are gone that we can use the excuse that SSL certificates are expensive to buy and install on your servers.

## Let’s Encrypt 

Let’s Encrypt is SSL certificate issuer that provides a free and automated generation process. It is possible to issue a certificate in less than a second without any registration process or payment.

On this blogpost I will show you how to deploy a very basic Go web application on the Cloud, running on HTTPS without paying a penny for it.

## Requirements

If you wish to follow this demo and try it yourself, please make sure you comply with following requirements.

- You need the Go compiler;
- A Virtual Machine publicly available (I'm using a Digital Ocean VM for this demo);
- A domain name and access to it’s DNS settings. You’ll not need this if your cloud provides a public shared name, for example: `yourvn0001.yourcloud.net`.

## Autocert

Autocert is a Go package that implements the ACME protocol used to generates certificates on Let’s Encrypt. This is the only external dependency you will need, no other install or package is required.

You can get it as any other Go package.

```
$ go get golang.org/x/crypto/acme/autocert
```

## The ~~magic~~ code explained step by step

```
package main

import (
	"crypto/tls"
	"fmt"
	"net/http"

	"golang.org/x/crypto/acme/autocert"
)


func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("/", func (w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello Secure World")
	})

	certManager := autocert.Manager{
		Prompt: autocert.AcceptTOS,
		Cache:  autocert.DirCache("certs"),
	}

	server := &http.Server{
		Addr:    ":443",
		Handler: mux,
	}
	server.TLSConfig = &tls.Config{
		GetCertificate: certManager.GetCertificate,
	}

	server.ListenAndServeTLS("", "")
}
```

We start the `main` function by creating a `mux` with a simple Hello World message on path `/`. In this example we're using the Go's default Mux, but it could be easily replaced by any other third-party router. 

The next step we create an instance of `autocert.Manager`. This struct is responsible for communicating with Let’s Encrypt and fetch the SSL certificates. The `Cache` field is an interface that defines on how and where `autocert.Manager` should store and load the certificates. In this example we're using `autocert.DirCache` that stores the certificates in a local folder. This is the easiest way to get started, but might not be best one for websites hosted on multiple servers, because each server will have it's own cache.

The last step is to create a `http.Server` that listen on port `443` and uses our `mux` instance. We then create `tls.Config` object and assign it to the server. Here's is probably **the most import part**. `GetCertificate` is a method that we can use to tell the server where to load the certificate whenever a new HTTPS is received. This method gives us the opportunity to choose what certificate to use instead of returning a specific one for every request. We then use `certManager.GetCertificate` which will try to get a matching certificate from the cache or fetch a new one from Let's Encrypt.

After that, all we need to do is start the server with `server.ListenAndServeTLS("", "")`. If you've worked with a HTTPS server on Go before, you probably remember that these two parameters are the `Certificate` and the `Privatey Key`. When using `autocert`, we don't need these so we pass an empty string.


## Let’s run it!

You can run it like any other Go web app, but if you do it on your local machine, it will fail. The reason being that Let’s Encrypt requires that the webiste to be publicly available through a know DNS name. When you run it locally, Let’s Encrypt cannot ping back your domain for verification purposes and it’ll fail miserably.

At this point I expect you to have your VM and your domain access. 

1) First of all, create a new DNS A record using your VM public IP.
2) Compile your Go app with `CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -o autossl` or different parameters if your target paltform is not linux/amd64
3) Copy your binary to the server.
4) Log into your server with SSH and start your Go server by simply running the binary you have just copied.
5) Open your browser with the address of your DNS domain.

![](/public/images/2017/11/auto-ssl-golang.png)

Ta-Da! You should now see the `Hello Secure World` message and the green SSL lock icon.

You might notice that it takes a while to load the first request. That’s because the SSL certificate generation process is happening on the background. Consecutive request should be lightning fast as the certificate is already cached.

## Important notes and suggestions:

1) There’s a limits on how many certificates can be generated for the same domain. At the time of this writing, the limit is 20 certificates per week. It might look a lot, but if you don’t manage your severs properly you might easily reach this limit.

2) It’s up to you to decided where and how you’re going to store the certificate. The temporary local storage works great when you have only a few servers. Once your cluster grows, this method might not suit you as each server will fetch a new certificate. Remember the limit I explained above.

3) The certificates are generated for 90 days. Thankfully autocert handles renewal automatically so you don’t need to do nothing. But it’s always a good practice to keep an eye on it when it’s close to expire date.

4) Try to use it on a multi-tenant application where each tenant has its own subdomain and you’ll fail. I learned the hard way. The limit is by domain name, excluding sub domain, so you can only generate certificates for 20 tenants a week.

5) As you cannot use autocert locally, you’ll need to build your app in such a way that you can choose between running on HTTP or HTTPS, so that you can easily use HTTP during development.

If you have any question, feedback or suggestion, please feel free to drop me a comment. I’m happy to help if I can.

Thanks!