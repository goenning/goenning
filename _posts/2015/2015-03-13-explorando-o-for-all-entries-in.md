---
title: Explorando o FOR ALL ENTRIES IN
layout: post
lang: pt
tags: [abap]
---
![](/public/images/2015/03/2014-05-05-20_57_24-Documentação-palavra-chave-ABAP.png)

No artigo [Open SQLsdf vs Native SQL](/2015/03/13/open-sql-vs-native-sql/) comentamos rapidamente sobre a instrução FOR ALL ENTRIES IN usada nos comandos Open SQL.

Resolvi escrever este outro artigo para explicar com mais detalhes o seu funcionamento e também mostrar como ela funciona por baixo dos panos, pois é uma instrução muito usada em ABAP.

## O QUE É

FOR ALL ENTRIES IN é um comando semelhante ao JOIN do SQL, a diferença é que enquanto estes são usados para fazer junções entre tabelas transparentes, o FOR ALL ENTRIES IN é usado para fazer filtros de uma tabela transparente usando uma tabela interna.

**Tabela transparente:** São as tabelas que existem fisicamente no banco de dados, possuem uma estrutura e seus dados são armazenados em disco.

**Tabela interna: **São as tabelas que existem somente durante o tempo de execução de um programa. Também possuem uma estrutura (muitas vezes a mesma da tabela transparente), porém os dados são armazenados em memória. Podemos chamá-las de tabela temporária, apesar deste nome não ser muito usado.

## COMO USAR

Para usar esta instrução precisamos primeiro definir uma tabela interna e preenchê-la com dados. Neste exemplo vou usar as tabelas SCARR e SPFLI que estão presentes tanto nos ambiente SAP oficiais quanto nas instalações do Mini SAP. Na tabela SCARR temos os registros de várias companhias aéreas enquanto que a SPFLI armazena a lista de todos os vôos oferecidos por estas companhias.

O código abaixo exemplifica o uso da instrução FOR ALL ENTRIES IN mostrando apenas os vôos oferecidos pelas companhias aéreas que trabalham com Dólar ou Euro.

~~~
REPORT Z_TESTE_FAE.

TYPES: BEGIN OF ty_carrier,
         id TYPE scarr-carrid,
         name TYPE scarr-carrname,
       END OF ty_carrier,
       BEGIN OF ty_flight,
         carrid TYPE scarr-carrid,
         connid TYPE spfli-connid,
         cityto TYPE spfli-cityto,
         cityfrom TYPE spfli-cityfrom,
         countryto TYPE spfli-countryto,
         countryfr TYPE spfli-countryfr,
       END OF ty_flight.

DATA: gt_carriers TYPE HASHED TABLE OF ty_carrier WITH UNIQUE KEY id,
      gt_flights TYPE SORTED TABLE OF ty_flight WITH NON-UNIQUE KEY carrid.

PERFORM f_carrega_dados.
IF gt_carriers[] IS NOT INITIAL.
  PERFORM f_imprime_dados.
ENDIF.

FORM f_carrega_dados.

  SELECT carrid carrname
    FROM scarr
    INTO TABLE gt_carriers
    WHERE currcode IN ('EUR', 'USD').

  IF gt_carriers[] IS NOT INITIAL.

    SELECT carrid connid cityto cityfrom countryto countryfr
      FROM spfli
      INTO TABLE gt_flights
      BYPASSING BUFFER
      FOR ALL ENTRIES IN gt_carriers
      WHERE carrid = gt_carriers-id.

  ENDIF.
ENDFORM.

FORM f_imprime_dados .

  DATA: from(30) TYPE c,
        to(30) TYPE c.

  FIELD-SYMBOLS: <fs_flight> TYPE ty_flight.

  LOOP AT gt_flights ASSIGNING <fs_flight>.

    FIELD-SYMBOLS: <fs_carrier> TYPE ty_carrier.

    AT NEW carrid.
      READ TABLE gt_carriers ASSIGNING <fs_carrier> WITH TABLE KEY id = <fs_flight>-carrid.
      FORMAT COLOR COL_HEADING.
      WRITE: / 'CARRIER: ', <fs_carrier>-name.
      FORMAT COLOR COL_NORMAL.
    ENDAT.

    CONCATENATE <fs_flight>-cityto <fs_flight>-countryto INTO to SEPARATED BY '-'.
    CONCATENATE <fs_flight>-cityfrom <fs_flight>-countryfr INTO from SEPARATED BY '-'.

    WRITE: / <fs_flight>-connid, from, to.

    AT END OF carrid.
      SKIP 1.
    ENDAT.
  ENDLOOP.

ENDFORM.
~~~

Até a **linha 17** temos apenas definições de estruturas, variáveis e tabelas internas. Note que estamos definindo tipos semelhantes aos da tabela, porém somente com os campos que tenho interesse. Isto é útil quando queremos reduzir a quantidade de informação retornada do banco de dados, pois diminui o trafego de rede e o consumo de memória. Usar **SELECT *** não é recomendado na maioria dos casos.

Nas **linha 26** estamos fazendo uma consulta para retornar o código e o nome das companhias que trabalham com Dólar ou Euro. O resultado da seleção é inserido na tabela interna **gt_carriers**.

A **linha 31** possui uma validação extremamente importante quando usamos FOR ALL ENTRIES IN. Vou explicar o motivo mais abaixo quando começarmos a falar do funcionamento interno.

Note na **linha 37 e 38** que a sintaxe é idêntica ao da instrução JOIN. Temos que informar qual o nome da tabela a qual queremos fazer a junção e quais são as condições.

Desta linha em diante são apenas instruções de LOOP e impressão de dados. Nas **linhas 54 e 59** estamos usando quebras para montar o cabeçalho da listagem.

Veja como ficou o resultado deste código.

![](/public/images/2015/03/exemplo-for-all-entries-in.png)

## COMO REALMENTE FUNCIONA

Já parou para pensar que nos comando SELECT onde usamos FOR ALL ENTRIES IN, metade dos dados estão em disco a outra metade está em memória? Como o ABAP faz para juntar isto?
  
Vamos usar a transação ST05 para ver o comando SQL que foi executado a partir deste Open SQL. Veja que colocamos a instrução **BYPASSING BUFFER** no comando de seleção, isto foi necessário pois esta tabela está com o buffer ligado, sem esta instrução não conseguiriamos coletar o trace do SQL.
  
Usando a transação ST05, estes são os dois comandos SQL que são executados.

![](/public/images/2015/03/sql-trace-1.png)

![](/public/images/2015/03/sql-trace-2.png)

O Database Interface converteu a instrução FOR ALL ENTRIES IN em uma condição WHERE com o operador IN. No meu ambiente todos as chaves da tabela interna foram passadas ao filtro, porém existem casos onde o ABAP faz um comando SQL para cada 5 chaves. Ou seja, para cada 5 registros na tabela interna é realizado um comando SELECT no banco. Não sei ao certo o motivo, talvez seja limitação de algum SGBD, mas me parece ser uma configuração de otimização realizada pelo BASIS.

Lembram que comentamos que é recomendável sempre adicionar uma validação que verifica se a tabela interna possui registros antes de usar FOR ALL ENTRIES IN? O motivo desta validação é que o Database Interface não adiciona a condição WHERE quando a tabela interna está vazia. Isto significa que corremos o risco de fazer uma consulta e retornar **TODOS** os dados da tabela transparente.

## E POR QUE NÃO USAR JOIN?

Este mesmo exemplo poderia ter sido feito com INNER JOIN e o resultado final teria sido o mesmo. Então quando devemos usar um ou outro?
  
Existem várias análises na internet para comparar o desempenho de um ou outro, mas este é apenas um dos fatores. Por via de regra, recomenda-se sempre usar JOIN ([veja mais informações aqui](http://scn.sap.com/thread/1174072)) quando possível. Também não é recomendado usar esta instrução quando a tabela interna for muito grande, pois pode gerar centenas ou milhares de comandos SQL separados.

Tomando como base o código acima, posso citar alguns motivos para usar FOR ALL ENTRIES no lugar do do JOIN.

  * Se tivéssemos usado JOIN a tabela interna teria o campo CARRNAME duplicado para cada registro de vôo. Isto é ruim, pois representa um consumo de memória e tráfego de rede desnecessário. No exemplo acima, o CARRNAME é retornado uma única vez para cada companhia.
  * O código ficou mais organizado com duas tabelas internas.
  * O código permite separar a seleção de companhias aéreas em um módulo de função. Nosso exemplo é bem simples, mas imagine que a seleção das companhias pudesse estar cheia de regras usadas em diversos pontos do sistema. A seleção poderia ser segregada do restante do código.
  * A tabela SPFLI possui o buffer ativo. Este benefício não seria utilizado caso utilizássemos JOIN.

O JOIN é muito útil quando precisamos buscar alguma informação complementar em uma única instrução SQL. Por exemplo, fazer uma consulta na tabela de itens de uma ordem de venda e buscar a descrição na tabela de materiais.

Não podemos definir uma regra para usar um ou outro, pois em muitos casos ambas as soluções são adequadas. Cabe a nós, enquanto programadores, avaliarmos caso a caso.