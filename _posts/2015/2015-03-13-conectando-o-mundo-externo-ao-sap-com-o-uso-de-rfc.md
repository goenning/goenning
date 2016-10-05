---
title: Conectando o mundo externo ao SAP com o uso de RFC
layout: post
comments: true
lang: pt
tags: [jco, módulo de função, nco, rfc, sap connector]
---
RFC é a interface padrão de comunicação entre sistemas SAP. É através deste protocolo que as informações são trocadas entre diferentes ambientes. Para que isto funcione é necessário configurar na transação SM59 o que são chamados de **RFC Destination**. Quando criamos uma RFC Destination é necessário informar as configurações técnicas, logon e também um nome. É este nome que é usado quando precisamos chamar uma função no sistema de destino.

~~~
REPORT zrfc_dest.

DATA: v_name(40) TYPE c.

CALL FUNCTION 'Z_BUSCADADOS'
  DESTINATION 'QAS'
  EXPORTING
    id   = 2
  IMPORTING
    name = v_name.
~~~

Neste exemplo estamos chamando a função **Z_BUSCADADOS** no sistema **QAS** que foi configurado na SM59. O código será executado no ambiente QAS e o seu resultado será retornado ao programa chamador.

## SAP e Sistemas Satélites

![](/public/images/2015/03/SAP_RFC.png)

Também é possível utilizar o protocolo RFC para integrar sistemas SAP com sistemas não-SAP. Apesar de ser um protocolo proprietário, a SAP disponibiliza bibliotecas para as principais linguagens de programação. Estas bibliotecas são chamadas de **SAP Connector**, sendo a JCo (Java Connector) e a NCo (.Net Connector) as duas mais conhecidas. Estes conectores podem ser baixados pelo portal de serviços da SAP. Para quem não tem permissão de download, na internet deve existir várias cópias, basta procurar pelos arquivos .JAR ou .DLL.

A interface de programação destas duas bibliotecas são bem semelhantes e fáceis de usar. Vamos explorar neste post um caso de estudo com código nas duas linguagens.

## Caso de Estudo

Para este tutorial vamos precisar criar uma função. Esta nossa função deverá receber o **ID** e retornar o **NOME** da companhia aérea. Caso não encontre nenhum registro, a exceção **CARR\_NOT\_FOUND** deve ser acionada.

A função ficou assim.

~~~
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
~~~

**IMPORTANTE:** Para que esta função possa ser chamada externamente é necessário habilitar o atributo **Remote-Enabled Function** na primeira aba da transação se37.

![](/public/images/2015/03/se37-rfc-header-info.png)

## Chamando a função pelo .Net

Criamos uma aplicação Console no Visual Studio e adicionamos referencia à 3 DLLs: **sapnco.dll**, **sapnco_utils.dll** e **rscp4n.dll**. Agora alteramos a classe **Program** para incluir o seguinte código.

~~~csharp
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
                    Console.WriteLine("Não foi encontrado uma Cia. Aérea com o código informado.");
            }
            Console.ReadLine();
        }
    }
}
~~~

Nas **linhas 10 até 17** são configurados os dados de conectividade com o SAP, incluindo nome da máquina, usuário, senha e outros parâmetros.
  
Na **linha 21** é feito uma conexão no SAP para buscar o metadado desta função. Com os metadados carregados é possível então informar os valores de entrada como na **linha 22**. A **linha 25** executa efetivamente a chamada da função enquanto a **linha 26** apenas obtêm o resultado. Colocamos também um block try/catch para capturar a possível exceção lançada pela função. Este exemplo foi só para mostrar o uso das exceções em RFC. Eu particularmente não gosto deste tipo de abordagem, prefiro muito mais criar um parâmetro de saída informando o status da execução (Succeso, Erro, Falha, etc) e uma tabela com uma lista de mensagens de erro.

A API de programação desta biblioteca é muito intuitiva e fácil de usar, concordam? Existem basicamente três interfaces que são muito usadas nesta biblioteca **IRfcFunction**, **IRfcTable** e **IRfcStructure**. É através destas interfaces que é feito o INPUT e OUTPUT das funções chamadas através da RFC.

## Chamando a função pelo Java

A chamada de RFC usando Java se da de forma bem semelhante ao .Net, as únicas diferenças são:

  * Os parâmetros de conexão com o SAP devem estar em arquivos com extensão **.jcoDestionation**.
  * É necessário adicionar ao build path o arquivo **sapjco3.jar** e a biblioteca nativa referente ao sistema operacional que está sendo usado. No caso do windows, usar **sapjco3.dll**. Para Unix, deve-se usar **sapjco3.so**.

Até mesmo a sintaxe é muito parecida. Veja só como ficou o arquivo de configuração e o programa main.

~~~
jco.client.lang=en
jco.client.client=001
jco.client.passwd=sapadmin2
jco.client.user=bcuser
jco.client.sysnr=00
jco.client.ashost=sap-vm
~~~

~~~java
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
                System.out.println("Não foi encontrado uma Cia. Aérea com o código informado.");
            }
        }
        System.out.println();
    }
}
~~~

Fácil, não é mesmo? Note que o nome do arquivo deve ser o mesmo que o parâmetro passado na função **getDestination**.

Isto é o básico que precisamos para conectar o mundo externo ao SAP. É claro que existem outras formas de acesso, como por exemplo o sistema de mensageria da SAP, conhecido como Process Integration.
  
Mas nunca é demais ter uma segunda opção, principalmente em se tratando de tecnologia, certo? Além disto, RFC tende a ser mais rápido que qualquer outra solução, afinal é a forma mais nativa de comunicação existente no SAP. Posso queimar a minha linguá afirmando isto, pois nunca fiz um teste, mas na teoria é isto que deveria acontecer.

### Bônus!

Para quem não tem acesso aos conectores SAP e também não tem uma instalação de SAP PI (que agora é SAP PO), existe uma terceira alternativa: SAP RFC via SOAP.
  
Mais informações no artigo </2015/03/13/conectando-se-ao-sap-com-o-uso-de-soap-rfc/>. Quanto mais opções, melhor!