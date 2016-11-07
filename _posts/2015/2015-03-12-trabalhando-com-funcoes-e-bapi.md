---
title: Trabalhando com Funções e BAPI
layout: post
comments: true
lang: pt
tags: [abap, bapi, function, se37]
---

No ABAP — assim como em diversas outras linguagens estruturadas — temos a possibilidade de criar pedaços de código que são reutilizáveis. Estes códigos são chamados de funções e estão disponíveis para que qualquer programa faça uso deles.

As funções são usadas quando queremos encapsular uma regra de negócio ou funcionalidade, sendo muito útil para manter a organização e controle do sistema. Criar uma função significa ter **um único ponto de manutenção** para aquela determinada rotina.

## BAPI

O SAP segue a risca este conceito e disponibiliza uma série de funções para manipular os dados do sistema. Estas funções são chamadas de **Business Application Programming Interface**, conhecidas simplesmente como **BAPI**. Imagine a situação onde desenvolvemos um sistema de digitação de pedidos para Tablet e queremos integrá-lo ao SAP. Quando o vendedor fizer uma venda no tablet é necessário criar uma ordem de venda no SAP. A VBAK é a tabela de OV, mas não temos segurança alguma para ir diretamente na tabela e executar um comando INSERT. Isto porque além desta tabela também deve haver uma outra dúzia de tabelas onde é necessário fazer alguma operação para que o pedido seja criado corretamente.

São casos como este que devemos usamos as BAPI. No caso da criação de ordem de venda devemos usar a função **BAPI\_SALESORDER\_CREATEFROMDAT2** ou um outra equivalente. Toda e qualquer transação que cria uma ordem de venda **precisa necessariamente** usar uma BAPI. É desta forma que o SAP garante a consistência dos dados de pedido.

Nós enquanto desenvolvedores devemos adotar uma postura semelhante à esta e criar funções para manipulação de dados de nossas rotinas, pois cada programa que faz um INSERT/UPDATE/DELETE diretamente em uma tabela é um potencial risco para a vida do sistema.

## Conhecendo o ambiente de trabalho

A transação **se37** é conhecida como **Function Builder** e é onde são criadas e testadas as funções ABAP. Toda função pertence à um **Function Group**. O nome deste componente já é auto explicativo, pois nada mais é que uma forma de agrupar as funções. Não vamos entrar no detalhe deste componente, mas é importante sempre criarmos grupos de funções separados para diferente contextos. Isto facilita o entendimento e organização dos objetos de função. Para mais informações sobre Grupos de Função, acesse [este endereço do help.sap](http://help.sap.com/saphelp_nw70/helpdata/en/9f/db992335c111d1829f0000e829fbfe/content.htm).

![](/public/images/2014/05/se37-header.png)

Uma função normalmente possui parâmetros de entrada e saída. No ABAP isto é um pouco diferente, pois temos mais tipos de parâmetros.

  * **Importing:** São os parâmetros de entrada e que devem ser preenchidas na chamada da função. Estes parâmetros podem ser opcionais ou não. 
  * **Exporting:** São os parâmetros de saída e que podem ou não ser usados pelo chamador da função. Estes parâmetros podem ser opcionais ou não.
  * **Changing:** São considerados parâmetros de entrada-saída. São preenchidas na chamada da função, alteradas durante a execução e retornadas para o chamador
  * **Tables:** É a forma antiga de se passar uma tabela de valores para a função. Atualmente este tipo de parâmetro está obsoleto, pois recomenda-se o uso de categorias de tabela.

Na aba **Exceptions** são definidas quais as possíveis exceções que a função pode lançar. Uma exceção é uma validação de negócio que impede que a função seja executada até o final. Se tivéssemos que criar uma função que divide dois números, uma possível exceção seria chamada de DIVISAO\_POR\_ZERO quando o divisor fosse igual a 0. Desta forma o programa chamador da função poderia fazer o devido tratamento do erro.

E é na última aba que nós fazemos a mágica acontecer. Aqui funciona como qualquer outro código fonte ABAP. Devemos trabalhar com os dados passados por parâmetro e no final preencher os valores nos parâmetros de saída ou modificação.

Veja como fica um exemplo da função que divide dois números e retornar o resultado juntamente com o resto da divisão.

~~~
FUNCTION zdivide.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_NRO1) TYPE  ZVALUE
*"     VALUE(I_NRO2) TYPE  ZVALUE
*"  EXPORTING
*"     VALUE(E_RESTO) TYPE  I
*"     VALUE(E_DIVISAO) TYPE  ZVALUE
*"  EXCEPTIONS
*"      DIVIDE_BY_ZERO
*"----------------------------------------------------------------------

if i_nro2 = 0.
  raise divide_by_zero.
endif.

  e_resto = i_nro1 MOD i_nro2.
  e_divisao = i_nro1 / i_nro2.


ENDFUNCTION.
~~~

**ZVALUE** é um elemento de dados do tipo **DEC 12,2**. O código é bem simples e por isto não requer explicação.

Agora que já temos a função criada, podemos chamá-la através de qualquer outro código fonte ABAP (Report, Função, Classe, etc) usando o comando CALL FUNCTION.
  
O código da chamada fica assim:

~~~
CALL FUNCTION 'ZDIVIDE'
  EXPORTING
    i_nro1         = num1
    i_nro2         = num2
  IMPORTING
    e_resto        = resto
    e_divisao      = result
  EXCEPTIONS
    divide_by_zero = 1
    OTHERS         = 2.

IF sy-subrc = 0.
  WRITE 'Divisao realizada com sucesso.'.
ELSEIF sy-subrc = 1.
  WRITE 'Divisor deve ser diferente de 0.'.
ELSEIF sy-subrc = 2.
  WRITE 'Ocorreu um erro desconhecido.'.
ENDIF.
~~~

**Dica:** Tenho como hábito sempre usar o botão **Pattern** quando preciso chamar uma função. Através deste botão é possível inserir um esqueleto da chamada da função já com todos os parâmetros e exceções. Isto agiliza bastante e evita erros de digitação, pois o verificador de sintaxe do ABAP não alerta quando digitamos errado o nome de um parâmetro.

![](/public/images/2015/03/callfunction-atalho.png)

E isto é tudo. Os objetos de função são bem simples de entender, até porque é muito semelhante com o que está disponível na maioria das outras linguagens.
  
O intuito deste artigo foi introduzir o início do assunto que será a pauta para as próximas publicações: **RFC**.