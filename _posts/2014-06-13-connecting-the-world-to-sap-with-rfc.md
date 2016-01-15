---
layout: post
title: Connecting the world to SAP with RFC
comments: true
tags: [sap, abap, rfc]
---

RFC is the standard interface of communication between SAP systems. It is through this protocol that data are exchanged between different environments. To make this work it is necessary to configure what is know as `RFC Destination`. A new destination can be created at `SM59` transaction, where we input technical configuration and a public name. This name is used when we need to call a function in a remote SAP system.

```abap
REPORT zrfc_dest.

DATA: v_name(40) TYPE c.

CALL FUNCTION 'Z_GET_DATA'
  DESTINATION 'QAS'
  EXPORTING
    id   = 2
  IMPORTING
    name = v_name.
```
In this example we are calling function `Z_GET_DATA` at `QAS` system which was previously configuration at SM59. This content of this function is evaluated and executed at the remote system and the result is returned to the caller.

## Exchanging data between SAP and any other application

![](/public/images/sap_rfc.png)

It's also possible to use the RFC protocol to integrate SAP systems with non-SAP systems. Although the protocol is proprietary, SAP offers libraries known as `SAP Connector` for the most used programming languages. The most notable are JCo (Java Connector) and NCo (.Net Connector). All connectors can be downloaded from [SAP Service Marketplace](http://service.sap.com/connectors).

The API of both libraries are very similar and easy to use.

Let's explore in this post a use case of both languages.

## Use case

For this tutorial we need a function that will be called from an external system and executed inside SAP.

Our function receives an ID and return the name of an airline company. In case of no records found, an exception named CARR_NOT_FOUND should be thrown.

The function looks like this.

```abap
FUNCTION z_get_scar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CARRID) TYPE  SCARR-CARRID
*"  EXPORTING
*"     VALUE(E_CARRNAME) TYPE  SCARR-CARRNAME
*"----------------------------------------------------------------------

  SELECT SINGLE carrname
    INTO e_carrname
    FROM scarr
    WHERE carrid = i_carrid.
  IF sy-subrc IS NOT INITIAL.
    raise CARR_NOT_FOUND.
  ENDIF.

ENDFUNCTION.
```

> DISCLAIMER: To be able to call this function remotely we need to enable the `Remote-Enabled Function` attribute at the first tab of `SE37` transaction.


![](/public/images/se37-rfc-header-info.png)

## Calling the ABAP function from .Net

First we create a new Console Application in Visual Studio and reference both assemblies from SAP NCo 3: `sapnco.dll` and `sapnco_utils.dll`.

Our `Program` class should be like this.

```abap
using System;
using SAP.Middleware.Connector;

namespace ConectandoSAP
{
    class Program
    {
        static void Main(string[] args)
        {
            RfcConfigParameters config = new RfcConfigParameters();
            config.Add(RfcConfigParameters.Name, "SAP");
            config.Add(RfcConfigParameters.AppServerHost, "sap-vm");
            config.Add(RfcConfigParameters.SystemNumber, "00");
            config.Add(RfcConfigParameters.User, "bcuser");
            config.Add(RfcConfigParameters.Password, "sapadmin2");
            config.Add(RfcConfigParameters.Client, "001");
            config.Add(RfcConfigParameters.Language, "EN");

            RfcDestination destination = RfcDestinationManager.GetDestination(config);
            RfcRepository repository = destination.Repository;
            IRfcFunction function = repository.CreateFunction("Z_GET_SCAR");
            function.SetValue("I_CARRID", "ZZZ");
            try
            {
                function.Invoke(destination);
                string name = function.GetString("E_CARRNAME");
                Console.WriteLine(name);
            }
            catch (RfcAbapException ex)
            {
                if (ex.Key == "CARR_NOT_FOUND")
                    Console.WriteLine("Airline company not found with given id.");
            }
            Console.ReadLine();
        }
    }
}
```

We start configuring the connection parameters for our `RfcDesination` instance.
The `CreateFunction` method makes a quick connection to SAP and return the metadata for the function passed by parameter, in this case, `Z_GET_SCAR`. The next few lines of code will only set the input parameters, invoke the function and print the output value. This example also demonstrates the use of exception in NCo, where there is a generic exception called `RfcAbapException` that we need to read the `Key` property in order to find which is the corresponding exception that was really thrown in ABAP. Although this works as expected, I'm not a big fan of it. I tend to use two output parameters for my RFC functions, a status field which tells the caller if the process ended with success or failure and a table of error messages.

This library's API is very easy to use, isn't it? There are basically three interfaces that are most used in the library: `IRfcFunction`, `IRfcTable` and `IRfcStructure`. It is through theses interfaces that happens the INPUT and OUTPUT of the remote function calls.

## Calling the ABAP function from Java

Calling from Java is almost the same as .Net, both libraries are very similar.

One of the few differences is that the configuration properties should be located in a `{Destination}.jcoDestionation` file rather than set in the code.
It is necessary to add java build path both `sapjco3.jar` and a operational system's native library. For Windows it is named `sapjco3.dll` and for Unix it is `sapjco3.so`.

The code syntax is very similar. This is how the configuration file and main Program looks like.

##### ABAP_AS.jcoDestionation
```ini
jco.client.lang=en
jco.client.client=001
jco.client.passwd=sapadmin2
jco.client.user=bcuser
jco.client.sysnr=00
jco.client.ashost=sap-vm
```

```java
package br.com.teste;

import com.sap.conn.jco.JCoDestination;
import com.sap.conn.jco.JCoDestinationManager;
import com.sap.conn.jco.JCoException;
import com.sap.conn.jco.JCoFunction;

public class Program {

	public static void main(String[] args) throws JCoException {

        JCoDestination destination = JCoDestinationManager.getDestination("ABAP_AS");
        JCoFunction function = destination.getRepository().getFunction("Z_GET_SCAR");

        function.getImportParameterList().setValue("I_CARRID", "ZZZ");
        try {
            function.execute(destination);
            System.out.println(function.getExportParameterList().getString("E_CARRNAME"));
        }
        catch (JCoException ex) {
            if (ex.getKey().equals("CARR_NOT_FOUND")) {
                System.out.println("Airline company not found with given id.");
            }
        }
        System.out.println();
    }
}
```

Easy, isn't it? Please note that the configuration filename should be the same as the destination named used in function `getDestination`.

This is the most simple example on how to connect both .Net and Java to SAP. There are others tools that can also be used to connect to SAP. One of them is called `SAP Process Integration`, mostly known as XI or PI.

RFC tends to be faster than any other option because it is the native method of communication through SAP systems.

Hope it helps.

See ya!