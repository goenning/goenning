---
title: Performance de leitura por tipo de tabela
layout: post
comments: true
lang: pt
tags: [ abap,  desempenho, performance, tabelas internas, tipos de tabela]
---
No artigo [os diferentes tipos de tabela do SAP](/2015/03/13/os-diferentes-tipos-de-tabela-do-sap/ "Os diferentes tipos de tabela do SAP") descrevemos a diferença dos três tipos de tabela interna: **Standard**, **Sorted** e **Hashed**.

O objetivo aqui é darmos um passo a mais e fazer uma análise mais profunda sobre a real diferença do tempo de leitura em cada uma destes tipos de tabela.

Para nos ajudar nesta tarefa resolvi criar um programa que simula um processo que utiliza tabelas internas de alto volume e grande quantidade de leituras. A ideia é identificar a partir de quantos registros é que o tipo de tabela influencia no resultado final.

## O cenário do teste e o programa de análise de performance

O cenário proposto para este teste é executar milhares de vezes as operações de **READ TABLE** em três tabelas internas com a mesma quantidade de registro, porém uma de cada tipo.
  
No total são cinco tipos de leitura.

  1. Standard Table com leitura simples
  2. Standard Table com binary search
  3. Standard Table com leitura por índice
  4. Sorted table
  5. Hashed table

São 18 baterias de teste onde são executados Y vezes a operação READ TABLE em cada uma das tabelas que possui X registros.
  
No primeiro teste X = 1000 e Y = 10, o segundo é X = 1000 e Y = 100 e assim por diante até chegar ao último teste onde X = 1000000 e Y = 1000000.

É desta forma que podemos estressar o sistema e descobrir em que ponto as tabelas indexadas começam a fazer diferença.

Estou disponibilizado o programa ao final do artigo caso tenha interesse em conhecer exatamente como foram executados os testes.
  
Fique a vontade para sugerir melhorias ou apontar algum problema que possa ter influenciado o resultado final.

## Os resultados obtidos

Esta é a tabela com o resultado do processamento do programa. Para termos dados mais confiáveis deixei a máquina totalmente ociosa durante a execução do programa.

![](/public/images//2015/03/zgdc_inttab_perf_test_result.png)

Na primeira coluna temos a quantidade de registros na tabela e a segunda coluna indica a quantidade de comandos READ TABLE foram executados. As outras colunas possuem o tempo em segundos para cada método. É facilmente percebido que o tempo de resposta da tabela standard começa a subir exponencialmente conforme aumenta o volume de dados, diferente das outras tabelas onde não houve discrepâncias. Contudo nota-se que READ TABLE &#8230; INDEX é o método mais rápido de todos, seguido de tabela HASHED com buscas pela chave completa. Na tabela não fica tão fácil identificar as diferenças de tempo com base no volume dos dados, então projetei este gráfico para facilitar a análise.

![](/public/images//2015/03/zgdc_inttab_perf_test_graph.png)

## O veredito

Todos os 5 métodos performaram **igualmente** bem até 10.000 registros em tabela com 1.000 leituras. Isto quer dizer que aquele seu programa que tem uma tabela hashed de 500 registros onde são feitos até 200 leituras poderia ser alterado para standard sem causar diferença perceptível. Com 10.000 leituras a tabela standard já apresentou problemas de lentidão e deixa de ser viável.

Os outros 4 métodos tiveram desempenho semelhante até o último teste, onde a diferença começa a aparecer. Standard table com índice é definitivamente o método mais eficaz, afinal é como se pegássemos um livro de milhares de páginas e sabemos exatamente em qual página está o conteúdo que procuramos, não é necessário olhar o índice do livro e muito menos folheá-lo. Mas convenhamos, em que situação é que temos um programa onde conseguimos acessar uma tabela pelo índice? É raro termos uma situação desta, mas se este for o caso do seu programa, use standard e faça bom uso desta vantagem. 

Em segundo lugar vem a tabela hashed que teve um desempenho semelhante, apesar de inferior. Usar sorted table ou standard table com binary search não faz diferença alguma, o desempenho é o mesmo, pois internamente o processo é idêntico.

O que podemos concluir aqui é que a não ser que você esteja trabalhando com um grande volume de dados, o método de leitura e o tipo de tabela não farão diferença. Apesar do gráfico mostrar uma diferença entre Hashed e Sorted a partir de 1.000.000 registros, a diferença é de apenas 0.2s. 

Apesar destas conclusões gosto sempre de começar os programas usando hashed table, pois é mais fácil começar um programa desta forma do que iniciar usando standard e depois ter que alterar. Faço isto também porque em um programa novo o tipo de tabela escolhido não influenciará no tempo total gasto para o desenvolvimento.

O que acharam do resultado? Ficou parecido com o que esperavam? Que tipo de tabelas vocês mais utilizam?

Utilize o código abaixo e execute em seu ambiente para ver se o resultado é semelhante.

## O código do programa de teste

Neste teste foi criado o report **ZGDC\_INTTAB\_PERF_TEST** com duas includes, **ZGDC\_INTTAB\_PERF\_TEST\_TOP** e **ZGDC\_INTTAB\_PERF\_TEST\_F01**.

~~~
INCLUDE zgdc_inttab_perf_test_top.
INCLUDE zgdc_inttab_perf_test_f01.

START-OF-SELECTION.

  " Uma tabela de cada tipo com uma quantidade de P_TBLCNT registros usadas para testar desempenho do comando READ.
  DATA: lt_standard TYPE ty_object_standard,
        lt_hashed   TYPE ty_object_hashed,
        lt_sorted   TYPE ty_object_sorted.

  DATA: lt_tempos          TYPE ty_tempo_tab.
  FIELD-SYMBOLS: <fs_tempo&gt; TYPE ty_tempo.


  " Tabela com uma quantidade de P_RDTCNT registros usada para armazenar os objetos que serão usados no READ em cada tabela.
  DATA: lt_objects_to_read TYPE ty_idx_object_tab.

  " Define quais serão as quantidades de registros e quantidade de leituras de cada teste
  PERFORM f_configura_cenarios CHANGING lt_tempos.

  " Coleta o resultado de processamento para cada cenário.
  LOOP AT lt_tempos ASSIGNING <fs_tempo&gt;.

    PERFORM f_preenche_tabelas CHANGING <fs_tempo&gt;-tblcnt
                                        <fs_tempo&gt;-rdtcnt
                                        lt_standard
                                        lt_hashed
                                        lt_sorted
                                        lt_objects_to_read.

    PERFORM f_teste_tabela_standard USING lt_objects_to_read
                                         lt_standard
                                CHANGING <fs_tempo&gt;.

    PERFORM f_teste_tabela_standard_binary USING lt_objects_to_read
                                                 lt_standard
                                        CHANGING <fs_tempo&gt;.

    PERFORM f_teste_tabela_standard_index USING lt_objects_to_read
                                               lt_standard
                                      CHANGING <fs_tempo&gt;.

    PERFORM f_teste_tabela_standard_sorted USING lt_objects_to_read
                                                 lt_sorted
                                        CHANGING <fs_tempo&gt;.

    PERFORM f_teste_tabela_standard_hashed USING lt_objects_to_read
                                                 lt_hashed
                                        CHANGING <fs_tempo&gt;.

  ENDLOOP.

  " Exibe os tempos em forma de ALV.
  PERFORM f_exibe_resultados USING lt_tempos.
~~~

~~~
REPORT  zgdc_inttab_perf_test.

TYPES:
  " Tipo de dado que irá ser armazenado
  BEGIN OF ty_object,
    pgmid	    TYPE pgmid,
    object    TYPE trobjtype,
    obj_name  TYPE sobj_name,
  END OF ty_object,

  " Tipos de tabela que serão testados
  ty_object_standard TYPE STANDARD TABLE OF ty_object,
  ty_object_hashed TYPE HASHED TABLE OF ty_object WITH UNIQUE KEY pgmid object obj_name,
  ty_object_sorted TYPE SORTED TABLE OF ty_object WITH UNIQUE KEY pgmid object obj_name,

  BEGIN OF ty_idx_object,
    index	TYPE syindex,
    object TYPE ty_object,
  END OF ty_idx_object,

  ty_idx_object_tab TYPE STANDARD TABLE OF ty_idx_object,

  BEGIN OF ty_tempo,
    " Quantidade total de registros nas tabelas a serem testadas: STANDARD, HASHED e SORTED
    tblcnt   TYPE i,
    " Quantidade total de READ TABLE que será executado nas tabelas
    rdtcnt   TYPE i,
    standard TYPE p DECIMALS 5,
    std_bin  TYPE p DECIMALS 5,
    std_idx  TYPE p DECIMALS 5,
    sorted   TYPE p DECIMALS 5,
    hashed   TYPE p DECIMALS 5,
  END OF ty_tempo,

  ty_tempo_tab TYPE STANDARD TABLE OF ty_tempo.
~~~

~~~
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_TABELAS
*&---------------------------------------------------------------------*
FORM f_preenche_tabelas USING uv_tblcnt          TYPE i
                              uv_rdtcnt          TYPE i
                     CHANGING ct_standard        TYPE ty_object_standard
                              ct_hashed          TYPE ty_object_hashed
                              ct_sorted          TYPE ty_object_sorted
                              ct_objects_to_read TYPE ty_idx_object_tab.

  DATA: ls_object     TYPE ty_object,
        ls_idx_object TYPE ty_idx_object.

  DATA: lo_rnd TYPE REF TO cl_abap_random,
        lv_num TYPE i.

  "Criando objeto LO_RND que será usado para obter números aleatórios.
  lo_rnd = cl_abap_random=&gt;create( ).

  SELECT pgmid
         object
         obj_name
    UP TO uv_tblcnt ROWS
    INTO TABLE ct_standard
    FROM tadir
   ORDER BY obj_name. "Deixando a tabela desordenada de propósito.

  ct_hashed = ct_standard.
  ct_sorted = ct_standard.

  "Procura UV_RDTCNT registros aleatórios e adiciona em nossa tabela que será base para o LOOP principal.
  DO uv_rdtcnt TIMES.
    lv_num = lo_rnd-&gt;intinrange( low = 1 high = uv_tblcnt ).
    READ TABLE ct_standard INDEX lv_num INTO ls_object.
    IF sy-subrc IS INITIAL.
      ls_idx_object-index = lv_num.
      ls_idx_object-object = ls_object.
      APPEND ls_idx_object TO ct_objects_to_read.
    ENDIF.
  ENDDO.
ENDFORM.                    " F_PREENCHE_TABELAS

*&---------------------------------------------------------------------*
*&      Form  F_TESTE_TABELA_STANDARD
*&---------------------------------------------------------------------*
FORM f_teste_tabela_standard USING value(ut_objects_to_read) TYPE ty_idx_object_tab
                                   value(ut_standard)        TYPE ty_object_standard
                          CHANGING cs_tempo                  TYPE ty_tempo.

  DATA: lv_timestamp_start TYPE timestampl,
        lv_timestamp_end   TYPE timestampl.

  FIELD-SYMBOLS: <fs_idx_object&gt; TYPE ty_idx_object.

  GET TIME STAMP FIELD lv_timestamp_start.

  LOOP AT ut_objects_to_read ASSIGNING <fs_idx_object&gt;.

    READ TABLE ut_standard TRANSPORTING NO FIELDS
    WITH KEY pgmid = <fs_idx_object&gt;-object-pgmid
             object = <fs_idx_object&gt;-object-object
             obj_name = <fs_idx_object&gt;-object-obj_name.

  ENDLOOP.

  GET TIME STAMP FIELD lv_timestamp_end.

  cs_tempo-standard = lv_timestamp_end - lv_timestamp_start.

ENDFORM.                    " F_TESTE_TABELA_STANDARD

*&---------------------------------------------------------------------*
*&      Form  F_TESTE_TABELA_STANDARD_BINARY
*&---------------------------------------------------------------------*
FORM f_teste_tabela_standard_binary USING value(ut_objects_to_read) TYPE ty_idx_object_tab
                                          value(ut_standard)        TYPE ty_object_standard
                                 CHANGING cs_tempo                  TYPE ty_tempo.

  DATA: lv_timestamp_start TYPE timestampl,
        lv_timestamp_end   TYPE timestampl.

  FIELD-SYMBOLS: <fs_idx_object&gt; TYPE ty_idx_object.
  SORT ut_standard BY pgmid object obj_name.

  GET TIME STAMP FIELD lv_timestamp_start.

  LOOP AT ut_objects_to_read ASSIGNING <fs_idx_object&gt;.

    READ TABLE ut_standard TRANSPORTING NO FIELDS
    WITH KEY pgmid = <fs_idx_object&gt;-object-pgmid
             object = <fs_idx_object&gt;-object-object
             obj_name = <fs_idx_object&gt;-object-obj_name BINARY SEARCH.

  ENDLOOP.

  GET TIME STAMP FIELD lv_timestamp_end.

  cs_tempo-std_bin = lv_timestamp_end - lv_timestamp_start.

ENDFORM.                    " F_TESTE_TABELA_STANDARD_BINARY

*&---------------------------------------------------------------------*
*&      Form  F_TESTE_TABELA_STANDARD_INDEX
*&---------------------------------------------------------------------*
FORM f_teste_tabela_standard_index USING value(ut_objects_to_read) TYPE ty_idx_object_tab
                                         value(ut_standard)        TYPE ty_object_standard
                                 CHANGING cs_tempo                 TYPE ty_tempo.

  DATA: lv_timestamp_start TYPE timestampl,
        lv_timestamp_end   TYPE timestampl.


  FIELD-SYMBOLS: <fs_idx_object&gt; TYPE ty_idx_object.
  SORT ut_standard BY pgmid object obj_name.

  GET TIME STAMP FIELD lv_timestamp_start.

  LOOP AT ut_objects_to_read ASSIGNING <fs_idx_object&gt;.

    READ TABLE ut_standard TRANSPORTING NO FIELDS INDEX <fs_idx_object&gt;-index.

  ENDLOOP.

  GET TIME STAMP FIELD lv_timestamp_end.

  cs_tempo-std_idx = lv_timestamp_end - lv_timestamp_start.

ENDFORM.          " F_TESTE_TABELA_STANDARD_INDEX

*&---------------------------------------------------------------------*
*&      Form  F_TESTE_TABELA_STANDARD_SORTED
*&---------------------------------------------------------------------*
FORM f_teste_tabela_standard_sorted USING value(ut_objects_to_read) TYPE ty_idx_object_tab
                                          value(ut_sorted)          TYPE ty_object_sorted
                                 CHANGING cs_tempo                  TYPE ty_tempo.

  DATA: lv_timestamp_start TYPE timestampl,
        lv_timestamp_end   TYPE timestampl.


  FIELD-SYMBOLS: <fs_idx_object&gt; TYPE ty_idx_object.

  GET TIME STAMP FIELD lv_timestamp_start.

  LOOP AT ut_objects_to_read ASSIGNING <fs_idx_object&gt;.

    READ TABLE ut_sorted TRANSPORTING NO FIELDS
    WITH KEY pgmid = <fs_idx_object&gt;-object-pgmid
             object = <fs_idx_object&gt;-object-object
             obj_name = <fs_idx_object&gt;-object-obj_name BINARY SEARCH.

  ENDLOOP.

  GET TIME STAMP FIELD lv_timestamp_end.

  cs_tempo-sorted = lv_timestamp_end - lv_timestamp_start.

ENDFORM.          " F_TESTE_TABELA_STANDARD_SORTED

*&---------------------------------------------------------------------*
*&      Form  F_TESTE_TABELA_STANDARD_HASHED
*&---------------------------------------------------------------------*
FORM f_teste_tabela_standard_hashed USING value(ut_objects_to_read) TYPE ty_idx_object_tab
                                          value(ut_hashed)          TYPE ty_object_hashed
                                 CHANGING cs_tempo                  TYPE ty_tempo.

  DATA: lv_timestamp_start TYPE timestampl,
        lv_timestamp_end   TYPE timestampl.


  FIELD-SYMBOLS: <fs_idx_object&gt; TYPE ty_idx_object.

  GET TIME STAMP FIELD lv_timestamp_start.

  LOOP AT ut_objects_to_read ASSIGNING <fs_idx_object&gt;.

    READ TABLE ut_hashed TRANSPORTING NO FIELDS
    WITH KEY pgmid = <fs_idx_object&gt;-object-pgmid
             object = <fs_idx_object&gt;-object-object
             obj_name = <fs_idx_object&gt;-object-obj_name.

  ENDLOOP.

  GET TIME STAMP FIELD lv_timestamp_end.

  cs_tempo-hashed = lv_timestamp_end - lv_timestamp_start.

ENDFORM.          " F_TESTE_TABELA_STANDARD_HASHED

*&---------------------------------------------------------------------*
*&      Form  F_CONFIGURA_CENARIOS
*&---------------------------------------------------------------------*
FORM f_configura_cenarios CHANGING ct_tempos TYPE ty_tempo_tab.
  DATA ls_tempo TYPE ty_tempo.

  ls_tempo-tblcnt = 1000.
  ls_tempo-rdtcnt = 1.

  DO 3 TIMES.
    ls_tempo-rdtcnt = ls_tempo-rdtcnt * 10.
    APPEND ls_tempo TO ct_tempos.
  ENDDO.

  ls_tempo-tblcnt = 10000.
  ls_tempo-rdtcnt = 1.

  DO 4 TIMES.
    ls_tempo-rdtcnt = ls_tempo-rdtcnt * 10.
    APPEND ls_tempo TO ct_tempos.
  ENDDO.

  ls_tempo-tblcnt = 100000.
  ls_tempo-rdtcnt = 1.

  DO 5 TIMES.
    ls_tempo-rdtcnt = ls_tempo-rdtcnt * 10.
    APPEND ls_tempo TO ct_tempos.
  ENDDO.

  ls_tempo-tblcnt = 1000000.
  ls_tempo-rdtcnt = 1.

  DO 6 TIMES.
    ls_tempo-rdtcnt = ls_tempo-rdtcnt * 10.
    APPEND ls_tempo TO ct_tempos.
  ENDDO.

ENDFORM.                    " F_CONFIGURA_CENARIOS

*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_RESULTADOS
*&---------------------------------------------------------------------*
FORM f_exibe_resultados USING ut_tempos TYPE ty_tempo_tab.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_field TYPE slis_fieldcat_alv.

  ls_field-tabname      = 'UT_TEMPOS'.

  ls_field-reptext_ddic = 'Table Count'.
  ls_field-fieldname    = 'TBLCNT'.
  APPEND ls_field TO lt_fieldcat.

  ls_field-reptext_ddic = 'Read Count'.
  ls_field-fieldname    = 'RDTCNT'.
  APPEND ls_field TO lt_fieldcat.

  ls_field-reptext_ddic = 'Standard'.
  ls_field-fieldname    = 'STANDARD'.
  APPEND ls_field TO lt_fieldcat.

  ls_field-reptext_ddic = 'Standard + Binary Search'.
  ls_field-fieldname    = 'STD_BIN'.
  APPEND ls_field TO lt_fieldcat.

  ls_field-reptext_ddic = 'Standard + IDX'.
  ls_field-fieldname    = 'STD_IDX'.
  APPEND ls_field TO lt_fieldcat.

  ls_field-reptext_ddic = 'Sorted'.
  ls_field-fieldname    = 'SORTED'.
  APPEND ls_field TO lt_fieldcat.

  ls_field-reptext_ddic = 'Hashed'.
  ls_field-fieldname    = 'HASHED'.
  APPEND ls_field TO lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat = lt_fieldcat
    TABLES
      t_outtab    = ut_tempos.

ENDFORM.
~~~