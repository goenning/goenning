---
layout: post
title: How to enable SAP ICF in a miniSAP installation
comments: true
lang: en
tags: [sap, icf]
---

If you ever try to access the ICF (Internet Communication Framework) Ping service in a newly installed miniSAP system, you'll probably get a 404 error page. Don't worry, the solution is quite simple.

## Ping (and most of other ICF services) are disabled by default

Enter `SICF` transaction and navigate through the tree until you reach `default_host/sap/bc/ping` node. Note that the service is disabled. Right click it and choose `Activate Service`. Now you can test it using your favorite internet browser. The default
address, which is `<computer-name>:8000/sap/bc/ping?sap-client=001`.

Still didn't work?

You'll need to find in which port your ICF is running. Right click the ping service and choose option `Test Service`. SAP will start your default browser and navigate to the ping service using the configured port. If you see port `0`, than you'll need to change it.

Find the file `C:\usr\sap\NSP\SYS\profile\DEFAULT.PFL` in your FileSystem and add these two lines to the bottom.

> icm/host_name_full = <nome-seu-servidor>
> icm/server_port_0 = PROT=HTTP,PORT=8000,TIMEOUT=3600,PROCTIMEOUT=3600

After that, restart your miniSAP system and try using the `Ping` service again.

If everything works as expected, your credentials will be prompted and the following message will be shown.

> Server reached successfully

This configuration is very important for a new feature that will be described in the following posts.

Cheers!
