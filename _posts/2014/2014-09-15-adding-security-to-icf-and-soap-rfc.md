---
layout: post
title: Adding security to ICF and SOAP RFC
lang: en
tags: [security, sap]
ref: adding-security-to-icf-and-soap-rfc
---

![](/public/images/no_auth_rfcping.png)

There is a risk when enabling and using RFC in a SAP environment.

SAP system ships with many useful functions with `Remote-Enabled Function` that are enabled by default. This mean any user with valid credentials can create unwanted Purchase Orders or Sales Orders by using the standard Remote-Enabled Functions. Or even worse, it is also possible to create a new users through RFC.

![](/public/images/bapi_create_user.png)

Fortunately SAP has an excellent built-in authorization module that also covers RFC. In this post you are going to learn how to protect your system from these unwanted RFC calls.

There are four authorization objects, but this post will only cover the two most used, `S_RFC` and `S_ICF`.
If you are new to authorization objects, this [SCN article](http://scn.sap.com/docs/DOC-17023) may come in handy.

First you got to understand how the `auth/rfc_authority_check` parameter works. This parameter defines in what level your SAP system will check for authority during a RFC.

The possible values are:

- **0:** No verification is done at all
- **1:** Verification is active, but only once for the same user context or function group. *This is the default configuration.*
- **2:** Verification is active, but only once for the function group.
- **3:** No authority verification is done, but it is necessary to login, except for calls to functions RFC_PING and RFC_SYSTEM_INFO
- **4:** Verification is active for all function calls except RFC_PING and RFC_SYSTEM_INFO
- **5:** No authority verification is done, but it is necessary to login, except for calls to function RFC_PING
- **6:** Verification is active for all function calls except RFC_PING
- **8:** No authority verification is done, but it is necessary to login for all functions
- **9:** Verification is active for all function calls

They all look like the same, there is only a small change between each other. For those who prefer code than documentation, look for function `AUTHORITY_CHECK_RFC`, this parameter is used inside it.

### S_RFC

This is the most used and important authorization object in profile configuration for RFC users. The usage is very simple, because there are only 3 authority parameters. It can be used on Function Module level or function group, it's your call. If it's set to a Function Module, only that function can be executed. When a Function Group is set, all functions inside it are allowed to be called. Multiples objects can be set in order to authorize multiple distinct functions.

*ACTVT:* Only option available is 16 – Executar.

*RFC_TYPE:* Possible values are FUGR ou FUNC, which represents Function Group and Function Module respectively.

*RFC_NAME:* This is where you can set the name of Function Group or Function Module, depending on what you set on RFC_TYPE.

*Tip:* To be able to call RFC functions using SAP Connector it is necessary to authorize some basic functions along side with business functions you may need. These functions are used to test the connection and to query metadata from function and structures. You can safely add these function groups `SYST`, `RFC1` e `RFC_METADATA`.

This is an example of a custom profile with RFC enabled. Z_SSRT is the function group that contains many functions that the user needs to call remotely.

![](/public/images/S_RFC.png)

Besides enabling RFC calls through S_RFC, most functions internally have additional authority check related to business processes, eg.: creating a sales order or changing a purchase order. You may need to add this objects to your profile as well.

### S_ICF

When doing a remote function call through SOAP Handler (ICF) there is no authority check at all. Any user (with correct credentials) can call any remote function through ICF without any additional permissions. S_RFC authority object is not used in this case. That's certainly not a good thing, but fortunately there is an authority object called S_ICF that come in handy. The problem is that it's a so simple mechanism that it is almost useless.

With S_ICF it is possible to define which ICF *Services* are enabled for each user, but it is still does not allow us to choose which *Functions* are enabled when using `sap/bc/soap/rfc` service.

To enable S_ICF, enter SICF transation and go to the desired service, which is sap/bc/soap/rfc in this case. Go to field `SAP Authoriz.` and input any value, eg.: RFC_CHECK. Edit the user profile and add the S_ICF authority object. Two parameters will be prompted: `ICF_FIELD` e `ICF_VALUE`. For ICF_FIELD input SERVICE and for ICF_VALUE input the same value used in SICF, in this case it is RFC_CHECK.

This configuration will tie users of this profile to services from SICF in which the field SAP Authoriz. is empty or equal to the ICF_VALUE parameter.

![](/public/images/sap_authoriz.png)

That's not the ideal solution, because it is not safe to allow all functions to be remotely executed. I found weird that there is no standard way of doing this kind of restriction.

I've then started debugging through class CL_HTTP_EXT_SOAPHANDLER_RFC and found something really useful.
There is a peace of code that checks wherever the function can be executed by looking at a white list table. The table is named `SRT_WHITE_LIST`, and when it's empty (and it is by default) all functions are allowed. When this table has at least a single row, then only the functions registered in this table are allowed to be called by SOAP.

![](/public/images/SRT_WHITE_LIST.png)

That is a real progress, but it's still not ideal, because this solution does not allow us to configuration funtions per user, this means that, once the function is white listed, any user with correct credentials will be able to remotely execute it. Combining S_CIF and SRT_WHITE_LIST we are very close to the perfect scenario. Better this than nothing, right?

That's all for now, hope you liked it.

Cheers!
