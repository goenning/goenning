---
layout: post
title: Connecting to SAP with SOAP RFC
lang: en
tags: [sap, abap, rfc, soap, icf, sharpsaprfc]
ref: connecting-to-sap-with-soap-rfc
---

In previous article entitled [How to enable SAP ICF in a miniSAP installation](/2014/08/01/how-to-enable-sap-icf-in-minisap-installation) you saw that SAP has a built-in HTTP module. This module contains multiples services, where each one has it's own interface and can be enabled or disabled depending on your needs. Each service has a corresponding ABAP Class that process the HTTP Response based on the HTTP Request. It is possible to identify the class by double-clicking at the service.

![](/public/images/class-handler-ping.png)

> CL_HTTP_EXT_PING is the class for `PING` service

It is also possible to publish your own HTTP Service by creating an ABAP Class that implements `IF_HTTP_EXTENSION` interface and register it by using `SICF` transaction. But this is not the subject of this post. What your are going to see here is how to use two very useful services: `sap/bc/soap/rfc` and `sap/bc/soap/wsdl`.

When used together, these services enables you to call remote-enabled function in SAP thought HTTP protocol.

### SOAP WSDL: sap/bc/soap/wsdl

This service returns the WSDL (Web Services Description Language) of a Remote-Enabled Function. This document contains all necessary information to call the function thought HTTP protocol. Many languages and programming platforms ships with helpers programs that generate code based on the WSDL file (this is commonly known as `Proxy`). One of the elements of the service description is called `endpoint`, and this is the address of the HTTP in which we need to use when calling the function.

### SOAP RFC: sap/bc/soap/rfc

This is the default endpoint for `bc/soap/wsdl`. This is the service that really executes the function based on the HTTP Request's body. The expected return content of this service is a XML content with the structures defined on WSDL and values returned from the function executed inside SAP.

## A pratical example

Suppose you have the following function named `Z_SSRT_SUM`.

~~~
FUNCTION z_ssrt_sum.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_NUM1) TYPE  INTEGER
*"     VALUE(I_NUM2) TYPE  INTEGER
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  INTEGER
*"----------------------------------------------------------------------

  e_result = i_num1 + i_num2.

ENDFUNCTION.
~~~

When you type `http://<server-name>:8000/sap/bc/soap/wsdl?sap-client=001&services=Z_SSRT_SUM` in the browser, your credentials will be prompted and the response text will be the following XML.

~~~XML
<definitions targetNamespace="urn:sap-com:document:sap:rfc:functions" xmlns="http://schemas.xmlsoap.org/wsdl/" xmlns:s0="urn:sap-com:document:sap:rfc:functions" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" xmlns:http="http://schemas.xmlsoap.org/wsdl/http/">
   <types>
      <xsd:schema targetNamespace="urn:sap-com:document:sap:rfc:functions">
         <xsd:element name="Z_SSRT_SUM">
            <xsd:complexType>
               <xsd:all>
                  <xsd:element name="I_NUM1" type="xsd:int"/>
                  <xsd:element name="I_NUM2" type="xsd:int"/>
               </xsd:all>
            </xsd:complexType>
         </xsd:element>
         <xsd:element name="Z_SSRT_SUM.Response">
            <xsd:complexType>
               <xsd:all>
                  <xsd:element name="E_RESULT" type="xsd:int"/>
               </xsd:all>
            </xsd:complexType>
         </xsd:element>
      </xsd:schema>
   </types>
   <message name="Z_SSRT_SUMInput">
      <part name="parameters" element="s0:Z_SSRT_SUM"/>
   </message>
   <message name="Z_SSRT_SUMOutput">
      <part name="parameters" element="s0:Z_SSRT_SUM.Response"/>
   </message>
   <portType name="Z_SSRT_SUMPortType">
      <operation name="Z_SSRT_SUM">
         <input message="s0:Z_SSRT_SUMInput"/>
         <output message="s0:Z_SSRT_SUMOutput"/>
      </operation>
   </portType>
   <binding name="Z_SSRT_SUMBinding" type="s0:Z_SSRT_SUMPortType">
      <soap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http"/>
      <operation name="Z_SSRT_SUM">
         <soap:operation soapAction="http://www.sap.com/Z_SSRT_SUM"/>
         <input>
            <soap:body use="literal"/>
         </input>
         <output>
            <soap:body use="literal"/>
         </output>
      </operation>
   </binding>
   <service name="Z_SSRT_SUMService">
      <documentation>SAP Service Z_SSRT_SUM via SOAP</documentation>
      <port name="Z_SSRT_SUMPortType" binding="s0:Z_SSRT_SUMBinding">
         <soap:address location="http://<server-name>:8000/sap/bc/soap/rfc"/>
      </port>
   </service>
</definitions>
~~~

See how `Z_SSRT_SUM` and `Z_SSRT_SUM.Response` elements are defined as the input and output of the `Z_SSRT_SUM` operation. Do the same for some other complex functions you may have in your system and note how easy it is to understand the standard of WSDL content generation.

## Testing the Service with soapUI

`soapUI` is a well known tool used to test WebServices without any need of programming. When you import the WSDL file, it'll automatically generate the follow HTTP Request body.

~~~XML
<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:urn="urn:sap-com:document:sap:rfc:functions">
   <soapenv:Header/>
   <soapenv:Body>
      <urn:Z_SSRT_SUM>
         <I_NUM1>?</I_NUM1>
         <I_NUM2>?</I_NUM2>
      </urn:Z_SSRT_SUM>
   </soapenv:Body>
</soapenv:Envelope>
~~~

Change both '?' to 2 and 5, and executed the request. The response will be like the following XML.

~~~XML
<SOAP-ENV:Envelope xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
   <SOAP-ENV:Body>
      <urn:Z_SSRT_SUM.Response xmlns:urn="urn:sap-com:document:sap:rfc:functions">
         <E_RESULT>7</E_RESULT>
      </urn:Z_SSRT_SUM.Response>
   </SOAP-ENV:Body>
</SOAP-ENV:Envelope>
~~~

> If it does not work for you, remember to set the `username` and `password` on the left sidebar at properties group named `Request Properties`.

## SharpSapRfc.Soap

I took advantage of to change the SharpSapRfc project and splitted it into two packages named `SharpSapRfc.Soap` and `SharpSapRfc.Plain.x86/x64`.

Both projects have the same usage API, that means it is possible to swap between `Plain Old RFC` and `SOAP RFC` without any breaking change. The major difference is when the connection class is instantiated, where each connector has it's own configuration properties.

I've found that the project has become well organized with two distinct implementations over the same interface. This is an example of how Oriented Object Programming can bring great benefits when used correctly.

More info and source code is available at [goenning/SharpSapRfc](https://github.com/goenning/SharpSapRfc).

Cheers!
