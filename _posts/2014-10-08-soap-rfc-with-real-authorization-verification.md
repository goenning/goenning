---
layout: post
title: SOAP RFC with real authorization verification
comments: true
tags: [sap, abap, rfc, soap]
---

If the system does not do something that you want, do it yourself! This is an advantage of working in a expansible platform. you can extend or modify standard behaviour to do anything (or almost anything) you need. I've first noticed the beauty of this when working with ASP.NET MVC and NHibernate. Know I feel the same about SAP.

The article [Connecting to SAP with SOAP RFC](/2014/08/20/connecting-to-sap-with-soap-rfc) explained how to use SOAP interface to call remote functions in SAP. The authorization objects and the white list table really comes in handy, but does not completely resolve our problem.

When thinking about how to solve this problem, something came to mind. Why don' we create our own SOAP interface? An interface that could actually run the authority check for the current user. That would be a really nice opportunity to learn how to create a custom ICF service that could be really useful.

I've defined the following premises for this exercise.

1. Maximize code reuse from standard handler. Doing a service of this magnitude is not straightforward and we don't want to mess with it.
2. Use the same security schema SAP does for plain RFC calls, that means we've get to use S_RFC authority object and the current user.
3. Does not alter by any means the XML interface of the service. We are just going to create a new endpoint for it, by doing so anyone can swap between both services without any code change.
4. Make good use of ABAP Objects.

## Let's get started

The first step is to create a new class that implements the interface `IF_HTTP_EXTENSION`. I've named my class as `Z_CL_HTTP_EXT_SOAPHANDLER_SRFC`, in which `SRFC` means Safe RFC. The only method that is inherited from IF_HTTP_EXTENSION is HANDLE_REQUEST. This method is responsible for processing an HTTP Request and creating the HTTP Response body.

Before jumping into the code it is necessary to include PERFINTERVAL and SOAPINCS at the global declarations of the class because we are going to need some methods that are bundled in these includes.

O primeiro passo é criar uma classe na transação se24 que implementa a interface IF_HTTP_EXTENSION. Batizei a minha classe de Z_CL_HTTP_EXT_SOAPHANDLER_SRFC, onde SRFC significa Safe RFC. Obrigatoriamente teremos que codificar o método HANDLE_REQUEST, herdado da IF_HTTP_EXTENSION. É neste método que montamos o XML que será retornando no corpo do response.
Antes de iniciar a codifição é necessário adicionar as includes PERFINTERVAL e SOAPINCS nas declarações globais da classe, pois precisaremos dos métodos contidos nestas includes.

```abap
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
include PERFINTERVAL.
INCLUDE SOAPINCS.
```

I've tend to create small and simple methods in my classes, I find that it makes it easier to understand. The following class is the full definition of our HTTP Handler class.

```abap
class Z_CL_HTTP_EXT_SOAPHANDLER_SRFC definition
  public
  final
  create public .

  public section.

    interfaces IF_HTTP_EXTENSION .
  protected section.
  private section.

    data RESPONSE type XSTRING .
    data SERVER type ref to IF_HTTP_SERVER .
    data REQUEST type XSTRING .

    methods RETURN_ERROR
      importing
        value(TITLE) type STRING
        value(TEXT) type STRING .
    methods GET_METHOD
      returning
        value(METHOD) type RS38L-NAME .
    methods CHECK_AUTHORITY
      importing
        value(FUNCTION_NAME) type RS38L-NAME
        value(FUNCTION_GROUP) type RS38L-AREA
      returning
        value(HAS_AUTHORITY) type OS_BOOLEAN .
ENDCLASS.
```
`RETURN_ERROR` is used when returning HTTP Status 500, which means something went wrong.
The method `GET_METHOD` is used to extract the function module name from the HTTP Requesty body.
`CHECK_AUTHORITY` is responsible for running the authority check using current user and target function.
`IF_HTTP_EXTENSION~HANDLE_REQUEST` bundles everything together and brings life to the service.

The following code shows the implementation of the HTTP Handler.

```abap
CLASS Z_CL_HTTP_EXT_SOAPHANDLER_SRFC IMPLEMENTATION.

  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method Z_CL_HTTP_EXT_SOAPHANDLER_SRFC->CHECK_AUTHORITY
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] FUNCTION_NAME                  TYPE        RS38L-NAME
  * | [--->] FUNCTION_GROUP                 TYPE        RS38L-AREA
  * | [<-()] HAS_AUTHORITY                  TYPE        OS_BOOLEAN
  * +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_authority.
    DATA: rfc_group  TYPE authb-rfc_name.

    rfc_group = function_group.
    CALL FUNCTION 'AUTHORITY_CHECK_RFC'
      EXPORTING
        userid             = sy-uname
        functiongroup      = rfc_group
        profilecheck       = 'X'
        functionname       = ''
      EXCEPTIONS
        function_not_exist = 1
        user_dont_exist    = 2
        rfc_no_authority   = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      CALL FUNCTION 'AUTHORITY_CHECK_RFC'
        EXPORTING
          userid             = sy-uname
          functiongroup      = ''
          functionname       = function_name
          profilecheck       = 'X'
        EXCEPTIONS
          function_not_exist = 1
          user_dont_exist    = 2
          rfc_no_authority   = 3
          OTHERS             = 4.
    ENDIF.

    IF sy-subrc = 0.
      has_authority = 'X'.
    ELSE.
      has_authority = ' '.
    ENDIF.

  ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method Z_CL_HTTP_EXT_SOAPHANDLER_SRFC->GET_METHOD
  * +-------------------------------------------------------------------------------------------------+
  * | [<-()] METHOD                         TYPE        RS38L-NAME
  * +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_method.

    DATA: my_soap_server_doc TYPE REF TO csoapserverdocument,
          method_name TYPE string.

    CREATE OBJECT my_soap_server_doc.

    CALL METHOD my_soap_server_doc->init
      EXPORTING
        input  = me->request
      CHANGING
        output = me->response.

    CALL METHOD my_soap_server_doc->get_method
      IMPORTING
        name = method_name.

    method = method_name.

  ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Public Method Z_CL_HTTP_EXT_SOAPHANDLER_SRFC->IF_HTTP_EXTENSION~HANDLE_REQUEST
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
  * +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_http_extension~handle_request.

    DATA: func_name  TYPE rs38l-name.
    DATA: func_group TYPE rs38l-area.
    DATA: has_authority TYPE abap_bool.

    me->server = server.
    me->request = me->server->request->get_data( ).
    func_name = me->get_method( ).

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = func_name
      IMPORTING
        group              = func_group
      EXCEPTIONS
        function_not_exist = 1.

    IF sy-subrc = 1.
      me->return_error( title = 'Function not found' text = `Function ` &&  func_name && ` does not exists.` ).
      RETURN.
    ENDIF.

    has_authority = me->check_authority( function_name = func_name function_group = func_group ).

    IF has_authority IS INITIAL.
      me->return_error( EXPORTING title = 'No authorization' text = `User ` && sy-uname && ` has no authority to execute function ` &&  func_name && `.` ).
      RETURN.
    ENDIF.

    DATA: soap_handler TYPE REF TO cl_http_ext_soaphandler_rfc.
    CREATE OBJECT soap_handler.
    CALL METHOD soap_handler->if_http_extension~handle_request
      EXPORTING
        server = me->server.

  ENDMETHOD.


  * <SIGNATURE>---------------------------------------------------------------------------------------+
  * | Instance Private Method Z_CL_HTTP_EXT_SOAPHANDLER_SRFC->RETURN_ERROR
  * +-------------------------------------------------------------------------------------------------+
  * | [--->] TITLE                          TYPE        STRING
  * | [--->] TEXT                           TYPE        STRING
  * +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD return_error.
    DATA: ofault  TYPE REF TO csoapfault.

    CALL METHOD csoapfault=>create_fault_from_parts
      EXPORTING
        fault_code   = 'Client'
        fault_string = title
        fault_detail = text
      IMPORTING
        new_fault    = ofault.

    CALL METHOD csoapserverdocument=>gen_fault_doc
      EXPORTING
        fault          = ofault
        fault_location = 1
      CHANGING
        fault_doc      = response.
    CALL METHOD server->response->set_status(
           code   = '500'                                     "#EC NOTEXT
           reason = 'Soap document processing failed' ).      "#EC NOTEXT
    CALL METHOD server->response->set_data( data = response ).
  ENDMETHOD.
ENDCLASS.
```

The code is self-explanatory, no need to explain. The cool part is where we create an instance of `CL_HTTP_EXT_SOAPHANDLER_RFC`. From this point on we leave all the complex processing to the standard service. When SAP releases a performance, security or any kind of update to this class, our custom service will take advantage of it as well.

![](/public/images/sicf-srfc.png)

The next step is to create the service and publish it to ICF.
This can be done at SICF transaction. Navigate to node `/default_host/sap/bc/soap/` and click on Create Host/Service button. Our new service will be installed side by side with the standard service. Name it `srfc` and selected Independent Service configuration. At Handler List tab, add the name of your class. Save and activate it.

You are now ready to test the service with soapUI.

When calling function Z_SSRT_ADD with our new service `/sap/bc/soap/srfc` and an unauthorized user gives the following response.

```XML
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/">
   <SOAP-ENV:Body>
      <SOAP-ENV:Fault>
         <faultcode>SOAP-ENV:Client</faultcode>
         <faultstring>No authorization</faultstring>
         <detail>User RFC_DENY has no authority to execute function Z_SSRT_ADD.</detail>
      </SOAP-ENV:Fault>
   </SOAP-ENV:Body>
</SOAP-ENV:Envelope>
```

With the same request and credentiais, if we change the endpoint of our request to `/sap/bc/soap/rfc`, the response is totally different.

```XML
<SOAP-ENV:Envelope xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
   <SOAP-ENV:Body>
      <urn:Z_SSRT_ADD.Response xmlns:urn="urn:sap-com:document:sap:rfc:functions">
         <C_NUM>3</C_NUM>
      </urn:Z_SSRT_ADD.Response>
   </SOAP-ENV:Body>
</SOAP-ENV:Envelope>
```

Cool, isn't it? It is now possible to disable the old service and leave only srfc enabled.

Cheers!