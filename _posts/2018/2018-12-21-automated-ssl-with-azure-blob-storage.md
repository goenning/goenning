---
layout: post
title: Using Azure Blob Storage as a cache backend for Go acme/autocert
comments: true
lang: en
tags: [go, azure, autocert]
description: By default, acme/autocert stores provisioned certificates on local disk for long-term caching. What happens is that the next time an user visits the same page, this package will fetch the certificate from the local disk instead of provisioning a new one. But what if your service is deployed across multiple machines? How do you ensure that all services are using the same cache? Come along and see how to solve this problem.
---

If you're not familiar with `acme/autocert` package on Go, I'd recommend you to start with [Free and Automated SSL Certificates with Go](https://goenning.net/2017/11/08/free-and-automated-ssl-certificates-with-go/) post. It'll show you how you can use acme/autocert to provision a Let's Encrypt Certificate for free in a fully automated manner.

By default, `acme/autocert` stores provisioned certificates on local disk for long-term caching. What happens is that the next time an user visits the same page, this package will fetch the certificate from the local disk instead of provisioning a new one (except if it needs to be renewed, but that's not relevant here).

The problem here is that if you have a cluster and your service is deployed across multiple machines, having a local disk cache is not very helpful as each machine would have its own cache. Sure you could use NFS to solve this, but there are more cloud-native ways of doing so.

Thankfully this package allow us to switch the cache strategy with a custom implementation where you can choose where cache the certificates.

### Azure Blob Storage

[github.com/goenning/azcertcache](https://github.com/goenning/azcertcache) is a Go package that implements a custom Cache strategy that allows you so store certificates in a [Azure Blob Storage](https://azure.microsoft.com/en-us/services/storage/blobs/) container.

To use this package you'll need the Account Name and Account Key that can be found here:

![](/public/images/2018/12/azbs-key.png)

And this is all the setup you need to do:

```go
containerName := "autocertcache"
cache, err := azcertcache.New("<account name>", "<account key>", containerName)
if err != nil {
  // Handle error
}

m := autocert.Manager{
  Prompt:     autocert.AcceptTOS,
  Cache:      cache, // <-- this used to be autocert.DirCache("<folder name>"),
}

s := &http.Server{
  Addr:      ":https",
  TLSConfig: &tls.Config{GetCertificate: m.GetCertificate},
}

s.ListenAndServeTLS("", "")
```

Ta-da! 🎉

The internal workflow is:

1. A requests is initialized for mysuperdomain.com
2. autocert checks if mysuperdomain.com certificate is in the in-memory cache and return it to client
3. if it's not found, autocert checks if mysuperdomain.com certificate is in the long-term cache, which in this case is Azure Blob Storage and return it to client
3. if it's not found, autocert fetches a new cerificates from let's encrypt and store in both in-memory and long-term cache.

**NOTE:** the in-memory cache is lost when the process restarts, hence why it's so important to keep these certificates on a long-term cache.

### BONUS!

What if you're not using Azure Blob Storage? Well then you still have at least three other options:

1. [https://github.com/goenning/sqlcertcache](https://github.com/goenning/sqlcertcache) to store certificates on a SQL Database
2. [https://github.com/danilobuerger/autocert-s3-cache](https://github.com/danilobuerger/autocert-s3-cache) to store certificates on a S3 compatible backend (like AWS S3, DigitalOcean Spaces, Self Hosted Minio...)
3. [NFS](https://en.wikipedia.org/wiki/Network_File_System) and continue to use `autocert.DirCache`.

Thanks!