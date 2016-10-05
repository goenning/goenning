---
title: SOAP RFC com verificação de autorização, agora sim!
layout: post
comments: true
lang: pt
tags: [sap, abap, rfc, soap]
ref: soap-rfc-with-real-authorization-verification
---

Se o sistema não faz exatamente o que você quer, senta a bunda na cadeira e **faça você mesmo**! Esta á uma das vantagem de se trabalhar em uma plataforma de fácil extensão — a possibilidade de sair da caixinha e buscar fazer algo diferente. Percebi isto primeiramente no ASP.NET MVC e NHibernate, agora estou vendo o mesmo acontecer com o SAP.

No [último artigo](/2015/03/14/adicionando-seguranca-no-uso-do-icf-e-rfc/ "Adicionando segurança no uso do ICF e RFC") discutimos sobre a falta de segurança por usuário no serviço RFC SOAP do SAP . Os objetos de autorização e a tabela de white list ajudam bastante, mas não resolvem completamente o problema.

Já no artigo [Conectando-se ao SAP com o uso de SOAP RFC](/2015/03/13/conectando-se-ao-sap-com-o-uso-de-soap-rfc/ "Conectando-se ao SAP com o uso de SOAP RFC") falamos rapidamente que é possível criar novos serviços do ICF implementando um classe conhecida como **Handler**. Após a escrita do post surgiu a ideia. Que tal fazermos um serviço ICF com suporte à autorização por usuário? Seria uma forma prática de aprender a criar um serviço e de quebrar fazer algo útil.

As premissas deste pequeno exercício são:

  * Reaproveitar ao máximo o código do handler padrão, pois o procedimento é complexo e alguém já quebrou a cabeça fazendo isto funcionar.
  * Utilizar o mesmo esquema de segurança da RFC padrão, ou seja, utilizar o objeto de autorização S_RFC e levar em conta o usuário que está executando a função.
  * Não alterar a estrutura XML do serviço, apenas criar um novo endereço web. Desta forma o usuário pode trocar de um serviço para o outro sem nenhuma alteração de código.
  * Fazer o bom uso de classes.

## Iniciando os trabalhos

O primeiro passo é criar uma classe na transação se24 que implementa a interface **IF\_HTTP\_EXTENSION**. Batizei a minha classe de **Z\_CL\_HTTP\_EXT\_SOAPHANDLER_SRFC**, onde SRFC significa **Safe RFC**. Obrigatoriamente teremos que codificar o método HANDLE\_REQUEST, herdado da IF\_HTTP_EXTENSION. É neste método que montamos o XML que será retornando no corpo do response.
  
Antes de iniciar a codifição é necessário adicionar as includes PERFINTERVAL e SOAPINCS nas declarações globais da classe, pois precisaremos dos métodos contidos nestas includes.

~~~
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
include PERFINTERVAL.
INCLUDE SOAPINCS.
~~~

Gosto muito de deixar minhas classes bem organizadas e de fácil entendimento, por isto sempre opto por codificar vários pequenos métodos. A interface da classe ficou assim.

~~~
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
~~~

Onde:

  * **RETURN_ERROR** é usado quando precisamos retornar o status HTTP 500, que significa que algo deu errado.
  * Usaremos o **GET_METHOD** para extrair qual o nome do módulo de função está sendo executado.
  * Já o método **CHECK_AUTHORITY** é responsável por verificar se o usuário requisitante possui autorização para executar a função desejada.
  * Tem também o **IF\_HTTP\_EXTENSION~HANDLE_REQUEST** que junta tudo isto e dá vida à classe.

Já a implementação ficou assim.

~~~
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
    me->return_error( title = 'Function not found' text = `Function ` &#038;&#038;  func_name &#038;&#038; ` does not exists.` ).
    RETURN.
  ENDIF.

  has_authority = me->check_authority( function_name = func_name function_group = func_group ).

  IF has_authority IS INITIAL.
    me->return_error( EXPORTING title = 'No authorization' text = `User ` &#038;&#038; sy-uname &#038;&#038; ` has no authority to execute function ` &#038;&#038;  func_name &#038;&#038; `.` ).
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
~~~

Não vou me ater à explicação do código, pois este fala por si só, afinal, está bem simples.
  
Para finalizarmos o teste só falta publicar esta classe em forma de serviço.

![](/public/images/2015/03/sicf-srfc.png)

Isto é feito na transação **SICF**, clicando no botão **Create Host/Service**. Mas antes disto é necessário selecionar o nó pai de onde desejamos que o novo serviço seja publicado. Neste teste selecionei o nó **/default_host/sap/bc/soap/**, pare que este fique lado a lado do serviço **rfc**.
  
Coloquei o nome de **srfc** e optei pela configuração **Independent Service**. Na aba Handler List simplesmente adicionei o nome da nossa classe na lista de classes.
  
Salve o serviço e ative-o. 

Pronto, agora já podemos fazer nossos testes com o SoapUI.

Este é um exemplo de retorno quando tentamos executar uma função com um usuário sem autorização.

~~~xml
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/">
   <SOAP-ENV:Body>
      <SOAP-ENV:Fault>
         <faultcode>SOAP-ENV:Client</faultcode>
         <faultstring>No authorization</faultstring>
         <detail>User RFC_DENY has no authority to execute function Z_SSRT_ADD.</detail>
      </SOAP-ENV:Fault>
   </SOAP-ENV:Body>
</SOAP-ENV:Envelope>
~~~

Se utilizarmos o mesmo conteúdo e usuário, porém apontando para o endereço **/default_host/sap/bc/soap/rfc**, o resultado será este.

~~~xml
<SOAP-ENV:Envelope xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
   <SOAP-ENV:Body>
      <urn:Z_SSRT_ADD.Response xmlns:urn="urn:sap-com:document:sap:rfc:functions">
         <C_NUM>3</C_NUM>
      </urn:Z_SSRT_ADD.Response>
   </SOAP-ENV:Body>
</SOAP-ENV:Envelope>
~~~

Então, gostaram? Querem mais um desafio? O serviço /default\_host/sap/bc/soap/wsdl retorna um WSDL onde o endpoint aponta para /default\_host/sap/bc/soap/.
  
Além disto, qualquer usuário pode usar este serviço para obter os metadados das funções, mesmo quem não tem acesso. A sugestão é criarmos o serviço **/swsdl**, responsável pela verificação de autorização e ainda retornaria o endpoint apontando para o serviço que criamos aqui. 

Por enquanto é isto, abraço!