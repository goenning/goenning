---
title: Conectando-se ao SAP com o uso de SOAP RFC
layout: post
lang: pt
tags: [sap, abap, rfc, soap, icf, sharpsaprfc]
ref: connecting-to-sap-with-soap-rfc
---

No artigo [Habilitando SAP ICF na instalação MiniSAP](/2015/03/14/habilitando-sap-icf-na-instalacao-minisap/ "Habilitando SAP ICF na instalação MiniSAP") falamos sobre o módulo HTTP que o SAP possui nativamente. Dentro deste módulo existem diversas interfaces que são chamadas de serviço. Cada serviço tem um objetivo e uma forma de funcionamento. Já vimos como funciona o **ping**, que é simplesmente um retorno HTML com uma mensagem de sucesso. Se clicamos no serviço podemos ver qual é a classe ABAP que é chamada quando fazemos uma requisição HTTP. Veja este exemplo do ping:

![](/public/images/2015/03/class-handler-ping.png)

Sabe o que isto significa? Podemos fazer nosso próprio serviço, basta criar uma classe ABAP que implementa a interface IF\_HTTP\_EXTENSION e registrá-la no SICF. Mas não é este o assunto deste post. O que vamos tratar aqui é de um serviço chamado **sap/bc/soap** e seus dois filhos **sap/bc/soap/rfc** e **sap/bc/soap/wsdl**. Estes serviços, quando usados em conjunto, servem para fazermos chamadas de função remote-enabled no SAP através de uma interface HTTP. 

![](/public/images/2015/03/sicf-bc-soap.png)

## SOAP WSDL: sap/bc/soap/wsdl

Este serviço retorna a especificação detalhada dos parâmetros de **input** e **output** de uma função remote-enabled. O formato retornado é o mundialmente conhecido WSDL (Web Services Description Language). Este documento contém todas as informações necessárias para fazermos uma chamada de função pelo protocolo HTTP. Muitas linguagens ou plataformas de programação possuem programas que geram códigos (conhecidos como Proxy) através de um arquivo WSDL, isto facilita bastante na hora de programar. Um dos elementos de um WSDL é o endpoint, que é o endereço HTTP para qual devemos fazer as chamadas.

## SOAP RFC: sap/bc/soap/rfc

Este é o endpoint padrão do WSDL do bc/soap/wsdl. É este serviço que efetivamente irá executa a função com os parâmetros que passarmos no corpo da requisição HTTP. O retorno será sempre um XML com a estrutura definidas no WSDL e com os valores processados pela função invocada no SAP. Bem simples, veja só o exemplo abaixo

## Um exemplo prático

Tenho uma função no SAP que é chamada de **Z\_SSRT\_SUM**. Veja só os parâmetros e código fonte dela.

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

Bem simples, apenas soma os parâmetros e retorna o resultado.

Se eu digitar o endereço **http://sap-vm:8000/sap/bc/soap/wsdl?sap-client=001&services=Z\_SSRT\_SUM** em meu navegador vai ser solicitado usuário e senha. Caso estejam corretos o WSDL será retornado.
  
O meu WSDL ficou assim:

~~~xml
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
         <input message="s0:Z_SSRT_SUMInput" />
         <output message="s0:Z_SSRT_SUMOutput"/>
      </operation>
   </portType>
   <binding name="Z_SSRT_SUMBinding" type="s0:Z_SSRT_SUMPortType">
      <soap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http"/>
      <operation name="Z_SSRT_SUM">
         <soap:operation soapAction="http://www.sap.com/Z_SSRT_SUM"/>
         <input />
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
         <soap:address location="http://sap-vm:8000/sap/bc/soap/rfc"/>
      </port>
   </service>
</definitions>
~~~

Veja que foram criados tipos complexos **Z\_SSRT\_SUM** e **Z\_SSRT\_SUM.Response** para definir os parâmetros de entrada e saída. Se nossa função usasse estruturas e tabelas, haveriam mais tags **types** descrevendo o formato destes tipos de dado. Faça isto com algumas de suas funções e logo vai perceber que existe um padrão para geração do WSDL, que inclusive é bem simples e fácil de entender.

O endpoint fica dentro da tag <service> ao final do conteúdo. Se tivéssemos HTTPS instalado haveriam dois endpoints, um para o HTTP e outro para HTTPS. 

Importei este WSDL no soapUI — uma excelente ferramenta para testar Web Services — e ele montou automaticamente o meu HTTP Request.

~~~xml
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

Alterei as duas interrogações por pelos números 2 e 5 e execute a função. O resultado ficou assim.

~~~xml
<SOAP-ENV:Envelope xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
   <SOAP-ENV:Body>
      <urn:Z_SSRT_SUM.Response xmlns:urn="urn:sap-com:document:sap:rfc:functions">
         <E_RESULT>7</E_RESULT>
      </urn:Z_SSRT_SUM.Response>
   </SOAP-ENV:Body>
</SOAP-ENV:Envelope>
~~~

Se em seu teste der falha ao fazer login não se esqueça de preencher os campos **Username** e **Password** na barra lateral esquerda do soapUI, chamada de Request Properties.

## SharpSapRfc.Soap

Eu não poderia perder a oportunidade e acabei fazendo um upgrade do [Sharp SAP RFC](/2015/03/14/novo-projeto-sharp-sap-rfc/). Separei em dois pacotes NuGet chamados de SharpSapRfc.Soap e SharpSapRfc.Plain.x86 (e x64 também). A interface de uso ficou exatamente a mesma, ou seja, é possível alterar entre Soap e Plain sem qualquer quebra no código. As únicas diferenças são na hora de instanciar a classe de conexão e o arquivo de configuração que também fica um pouco diferente. 

Achei que projeto ficou bem organizado tendo duas implementações distintas para a mesma interface. Foi exemplo para mim de que a orientação a objetos quando usada de forma correta pode trazer grandes benefícios. Foi usado muito polimorfismo no projeto e vejo que pode ser útil como material de estudo.

Mais informações em <https://www.nuget.org/packages/SharpSapRfc/>.

## E tem mais!

Eu já li algumas vezes que é possível publicar uma função da se37 em forma de Web Service sem ter que programar nada. Eu nunca fui atrás, mas serve como dica para quem está procurando algo do gênero. Sem nunca ter usado, a única vantagem que eu consigo ver no momento é com relação à segurança, pois podemos disponibilizar para HTTP somente as funções que realmente precisamos. O ICF bc/soap/rfc é bem irrestrito neste quesito, podemos chamar qualquer função remote-enabled através dele. Em verdade, em verdade eu vos digo, existe uma forma de controlar a segurança neste serviço também, mas é assunto para outro post.

Abraços!