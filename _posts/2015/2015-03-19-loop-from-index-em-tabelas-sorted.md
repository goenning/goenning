---
title: LOOP FROM INDEX em tabelas sorted
layout: post
comments: true
lang: pt
tags: [abap, loop, performance, tipos de tabela]
---
No artigo [Performance de leitura por tipo de tabela](/2015/03/18/performance-de-leitura-por-tipo-de-tabela/ "Performance de leitura por tipo de tabela") descobrimos a real diferença no tempo de exeucução para leituras nos três tipos de tabela: standard, sorted e hashed. Neste exemplo em questão fizemos os testes somente com a operação de READ TABLE que é usada quando precisamos de apenas um registro da tabela. Quando é necessário operar com um conjunto de registros de uma tabela interna precisamos usar LOOP&#8230;WHERE. Em termos de desempenho o resultado é semelhante, pois o LOOP nada mais é que diversas operações de READ até ter lido todos os registros que satisfazem a condição especificado no filtro WHERE. Mas existe uma técnica conhecida como **LOOP FROM INDEX** que promete diminuir o tempo de leitura nestas situações

Concluímos no artigo em questão que o acesso mais rápido possível é quando usamos o índice. Neste caso o SAP sabe exatamente onde está o registro e o tempo de processamento é reduzido drasticamente. A técnica que vamos ver aqui é justamente uma combinação de LOOP com acesso por índice, conforme mostra a documentação do LOOP AT.

![](/public/images//2015/03/abapdocu-loop-at-from-index.png)

Vamos mais uma vez criar um programa e analisar o resultado do LOOP pelas chaves e outro pelo índice, mas antes vamos entender como funciona o LOOP FROM INDEX.

## LOOP FROM INDEX

Os pré-requisito para utilizar este método é que a tabela interna precisa estar ordenada — seja através do comando SORT ou por estar definida como SORTED — e que o LOOP pretendido seja pelos campos que estão ordenados (o que normalmente é verdade). Apesar de não ser um pré-requisito, é comum encontrarmos esta técnica sendo utilizada em casos onde tempos LOOP dentro de LOOP, pois nestes casos o benefício é proporcional à quantidade de vezes que o LOOP interno será executado.

A técnica consiste em descobrir em qual índice está o primeiro registro que precisamos e em seguida fazer um LOOP a partir deste índice até encontrarmos o primeiro registro com chave diferente. Confuso? Então veja o seguinte código, deve ajudar a clarear as ideias.

O seguinte trecho de código mostra o método tradicional, que é o mais conhecido e o mais usado.

~~~
LOOP AT lt_pedidos ASSIGNING <fs_pedido&gt;.
  LOOP AT lt_itens assgning <fs_item&gt; WHERE cod_pedido = <fs_pedido&gt;-cod_pedido.
    "Faça algo com os itens deste pedido.
  ENDLOOP.
ENDLOOP.
~~~

E este código aqui tem o mesmo resultado, porém de uma forma mais complicada e teoricamente mais rápido.

~~~
LOOP AT lt_pedidos ASSIGNING <fs_pedido&gt;.

  READ TABLE lt_itens WITH KEY cod_pedido = <fs_pedido&gt;-cod_pedido BINARY SEARCH TRANSPORTING NO FIELDS.
  CHECK sy-subrc = 0.

  LOOP AT lt_itens FROM sy-tabix ASSIGNING <fs_item&gt;.
    "Faça algo com os itens deste pedido.

    IF <fs_item&gt;-cod_pedido <> <fs_pedido&gt;-cod_pedido.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDLOOP.
~~~

Estamos trocando um código simples e fácil de entender por um código maior e mais complicado. Será que vale a pena? É isto que vamos tentar desvendar aqui.

## Análise de diferença de desempenho

Criei um programa que trabalha com duas tabelas internas, uma tabela pai e outra filha. Em cada bateria de teste uma quantidade X de registros é inserido na tabela pai e Y registros são gerados na tabela filha para cada registro pai. No final temos uma tabela filha que é Y vezes maior que a tabela pai. São 20 baterias de teste, onde o menor cenário é quando X = 500 e Y = 100 e o maior é X = 5000 e Y = 1000.

São nestas duas tabelas que são coletados os tempos de leitura através dos dois métodos: LOOP&#8230;WHERE e LOOP&#8230;FROM INDEX.

Curioso para saber o resultado? Então aqui está:

![](/public/images//2015/03/zgdc_loop_from_index_result.png)
![](/public/images//2015/03/zgdc_loop_from_index_graph.png)

Quanto mais para a direita, maior a quantidade de registros em cada tabela. Para os teste onde a tabela filha tinha 100x a quantidade de registros da tabela pai a diferença entre os dois métodos foi pequena, quase imperceptível. Agora quando aumentamos a quantidade para 1000x começamos a notar que o LOOP FROM INDEX apresenta um menor tempo de processamento na medida que a tabela pai também cresce. 

Veja que no final a diferença total entre os dois métodos é de apenas **0,2 segundos**. Estamos falando de uma tabela de 5.000 registros e outra de 5.000.000, o que não é muito comum na maioria dos programas. 

Então me diga, vale a pena complicar nosso código por conta destes poucos milissegundos? Sinceramente, se o seu programa está lento não é isto que irá fazer a diferença. Invista seu tempo e esforço nos problemas que realmente trarão benefícios, certamente este não é um dos problemas.

## O código do programa de teste

Neste teste foi criado o report **ZGDC\_LOOP\_FROM_INDEX** com duas includes, **ZGDC\_LOOP\_FROM\_INDEX\_TOP** e **ZGDC\_LOOP\_FROM\_INDEX\_F01**.

~~~
INCLUDE zgdc_loop_from_index_top.
INCLUDE zgdc_loop_from_index_f01.

START-OF-SELECTION.

  DATA: lt_objects TYPE ty_object_tab,
        lt_children TYPE ty_child_tab.

  DATA: lt_tempos          TYPE ty_tempo_tab.
  FIELD-SYMBOLS: <fs_tempo&gt; TYPE ty_tempo.

  " Define quais serão as quantidades de registros e quantidade de leituras de cada teste
  PERFORM f_configura_cenarios CHANGING lt_tempos.

  LOOP AT lt_tempos ASSIGNING <fs_tempo&gt;.

    PERFORM f_preenche_tabelas CHANGING <fs_tempo&gt;-tblcnt
                                        <fs_tempo&gt;-cldcnt
                                        lt_objects
                                        lt_children.

    PERFORM f_teste_loop_where USING lt_objects
                                     lt_children
                            CHANGING <fs_tempo&gt;.

    PERFORM f_teste_loop_from_index USING lt_objects
                                          lt_children
                                 CHANGING <fs_tempo&gt;.
  ENDLOOP.

  " Exibe os tempos em forma de ALV.
  PERFORM f_exibe_resultados USING lt_tempos.
~~~

~~~
REPORT zgdc_loop_from_index.

TYPES:
  BEGIN OF ty_object,
    pgmid	    TYPE pgmid,
    object    TYPE trobjtype,
    obj_name  TYPE sobj_name,
  END OF ty_object,
  BEGIN OF ty_object_child,
    pgmid	    TYPE pgmid,
    object    TYPE trobjtype,
    obj_name  TYPE sobj_name,
    position  TYPE i,
  END OF ty_object_child,

  ty_object_tab TYPE STANDARD TABLE OF ty_object,
  ty_child_tab TYPE SORTED TABLE OF ty_object_child WITH UNIQUE KEY pgmid object obj_name position,

  BEGIN OF ty_tempo,
    " Quantidade total de registros na tabelas pai
    tblcnt      TYPE i,
    " Quantidade total de registros na tabela filha para cada registro pai
    cldcnt      TYPE i,
    loop_where  TYPE p DECIMALS 5,
    from_index  TYPE p DECIMALS 5,
  END OF ty_tempo,

  ty_tempo_tab TYPE STANDARD TABLE OF ty_tempo.
~~~

~~~
*&---------------------------------------------------------------------*
*&      Form  F_CONFIGURA_CENARIOS
*&---------------------------------------------------------------------*
FORM f_configura_cenarios CHANGING ct_tempos TYPE ty_tempo_tab.
  DATA ls_tempo TYPE ty_tempo.

  ls_tempo-tblcnt = 0.

  DO 10 TIMES.
    ls_tempo-tblcnt = ls_tempo-tblcnt + 500.
    ls_tempo-cldcnt = 10.

    DO 2 TIMES.
      ls_tempo-cldcnt = ls_tempo-cldcnt * 10.
      APPEND ls_tempo TO ct_tempos.
    ENDDO.
  ENDDO.
ENDFORM.                    " F_CONFIGURA_CENARIOS

*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_TABELAS
*&---------------------------------------------------------------------*
FORM f_preenche_tabelas  USING uv_tblcnt   TYPE i
                               uv_cldcnt   TYPE i
                      CHANGING ct_objects  TYPE ty_object_tab
                               ct_children TYPE ty_child_tab.
  FREE: ct_objects, ct_children.

  FIELD-SYMBOLS: <fs_object&gt; TYPE ty_object.
  DATA: ls_child TYPE ty_object_child.

  "Obtemos uma quantidade UV_TBLCNT registros para nossa tabela pai.
  SELECT pgmid
         object
         obj_name
    UP TO uv_tblcnt ROWS
    INTO TABLE ct_objects
    FROM tadir.

  "para cada registro da tabela pai, criamos mais UV_CLDCNT registros na tabela filho.
  LOOP AT ct_objects ASSIGNING <fs_object&gt;.
    DO uv_cldcnt TIMES.
      ls_child-pgmid = <fs_object&gt;-pgmid.
      ls_child-object = <fs_object&gt;-object.
      ls_child-obj_name = <fs_object&gt;-obj_name.
      ls_child-position = sy-index.
      INSERT ls_child INTO TABLE ct_children.
    ENDDO.
  ENDLOOP.

  "Reordenamos a tabela pai para que ela fique com ordenação diferente da tabela filha
  SORT ct_objects BY obj_name.
ENDFORM.                    " F_PREENCHE_TABELAS

*&---------------------------------------------------------------------*
*&      Form  F_TESTE_LOOP_WHERE
*&---------------------------------------------------------------------*
FORM f_teste_loop_where USING ut_objects  TYPE ty_object_tab
                              ut_children TYPE ty_child_tab
                     CHANGING ct_tempo    TYPE ty_tempo.

  DATA: lv_timestamp_start TYPE timestampl,
        lv_timestamp_end   TYPE timestampl.

  FIELD-SYMBOLS: <fs_object&gt; TYPE ty_object,
                 <fs_child&gt;  TYPE ty_object_child.

  GET TIME STAMP FIELD lv_timestamp_start.

  LOOP AT ut_objects ASSIGNING <fs_object&gt;.
    LOOP AT ut_children ASSIGNING <fs_child&gt;
                      WHERE pgmid = <fs_object&gt;-pgmid
                        AND object = <fs_object&gt;-object
                        AND obj_name = <fs_object&gt;-obj_name.

    ENDLOOP.
  ENDLOOP.

  GET TIME STAMP FIELD lv_timestamp_end.
  ct_tempo-loop_where = lv_timestamp_end - lv_timestamp_start.

ENDFORM.                    " F_TESTE_LOOP_WHERE

*&---------------------------------------------------------------------*
*&      Form  F_TESTE_LOOP_FROM_INDEX
*&---------------------------------------------------------------------*
FORM f_teste_loop_from_index USING ut_objects  TYPE ty_object_tab
                                   ut_children TYPE ty_child_tab
                          CHANGING ct_tempo    TYPE ty_tempo.

  DATA: lv_timestamp_start TYPE timestampl,
        lv_timestamp_end   TYPE timestampl.

  FIELD-SYMBOLS: <fs_object&gt; TYPE ty_object,
                 <fs_child&gt;  TYPE ty_object_child.

  GET TIME STAMP FIELD lv_timestamp_start.

  LOOP AT ut_objects ASSIGNING <fs_object&gt;.
    READ TABLE ut_children WITH KEY pgmid = <fs_object&gt;-pgmid
                                    object = <fs_object&gt;-object
                                    obj_name = <fs_object&gt;-obj_name BINARY SEARCH TRANSPORTING NO FIELDS.
    CHECK sy-subrc = 0.

    LOOP AT ut_children FROM sy-tabix ASSIGNING <fs_child&gt;.

      IF <fs_child&gt;-pgmid <&gt; <fs_object&gt;-pgmid OR
         <fs_child&gt;-object <&gt; <fs_object&gt;-object OR
         <fs_child&gt;-obj_name <&gt; <fs_object&gt;-obj_name.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  GET TIME STAMP FIELD lv_timestamp_end.
  ct_tempo-from_index = lv_timestamp_end - lv_timestamp_start.

ENDFORM.                    " F_TESTE_LOOP_FROM_INDEX

*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_RESULTADOS
*&---------------------------------------------------------------------*
FORM f_exibe_resultados USING ut_tempos TYPE ty_tempo_tab.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_field TYPE slis_fieldcat_alv.

  ls_field-tabname      = 'UT_TEMPOS'.

  ls_field-reptext_ddic = 'Parent Count'.
  ls_field-fieldname    = 'TBLCNT'.
  APPEND ls_field TO lt_fieldcat.

  ls_field-reptext_ddic = 'Child Count'.
  ls_field-fieldname    = 'CLDCNT'.
  APPEND ls_field TO lt_fieldcat.

  ls_field-reptext_ddic = 'LOOP...WHERE'.
  ls_field-fieldname    = 'LOOP_WHERE'.
  APPEND ls_field TO lt_fieldcat.

  ls_field-reptext_ddic = 'LOOP...FROM INDEX'.
  ls_field-fieldname    = 'FROM_INDEX'.
  APPEND ls_field TO lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat = lt_fieldcat
    TABLES
      t_outtab    = ut_tempos.

ENDFORM.
~~~