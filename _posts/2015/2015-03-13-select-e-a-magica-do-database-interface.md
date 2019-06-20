---
title: 'SELECT * e a mágica do Database Interface'
layout: post
lang: pt
tags: [abap]
ref: select-all-and-the-magic-behind-database-interface
---

Falamos [no artigo sobre OpenSQL](/2015/03/13/open-sql-vs-native-sql/ "Open SQL vs Native SQL") que o Database Interface atua no meio de campo entre a aplicação e o banco de dados, lembram? Ele é responsável por algumas tarefas como a tradução de OpenSQL em NativeSQL, manutenção dos dados no buffer, entre outras.

Recentemente descobri algo novo que ele também faz e que é saudável para o banco de dados e consequentemente o ambiente de um modo geral.

Sempre defendi (e continuo defendendo) que comandos **SELECT *** devem ser evitados ao máximo, pois representam um risco para o desempenho da aplicação por interferirem diretamente em múltiplas camadas, principalmente na rede.

Pois bem, acontece que o SAP é bem inteligente e acaba fazendo diversas otimizações para nós. Uma delas é justamente na seleção de colunas desnecessárias.
  
Vamos montar três exemplos, um usando **SELECT fieldlist** e outros dois usando **SELECT ***, onde um deles preenche uma tabela com menos campos.

~~~
TYPES: BEGIN OF ty_field,
        tabname TYPE dd03l-tabname,
        fieldname TYPE dd03l-fieldname,
       END OF ty_field.

DATA: gt_fields1 TYPE TABLE OF ty_field.
DATA: gt_fields2 TYPE TABLE OF ty_field.
DATA: gt_fields3 TYPE TABLE OF dd03l.

SELECT tabname fieldname
  FROM dd03l
  INTO TABLE gt_fields1
  WHERE tabname = 'PARTNER_RECORD'.

SELECT *
  FROM dd03l
  INTO CORRESPONDING FIELDS OF TABLE gt_fields2
  WHERE tabname = 'PARTNER_RECORD'.

SELECT *
  FROM dd03l
  INTO TABLE gt_fields3
  WHERE tabname = 'PARTNER_RECORD'.
~~~

Quando possível sempre opto pelo primeiro exemplo onde informamos campo a campo tanto na definição da estrutura quanto no comando de seleção. Acredito que torna o código mais simples de ler e também mais otimizado para leituras no banco de dados.
  
Se executarmos o SQL Trace (ST05) deste programa podemos notar que o tempo de execução no banco de dados foi o mesmo para os dois primeiros e um pouco mais demorado para o terceiro caso. Isto quer dizer que a prática contrariou a teoria? 

![](/public/images/2015/03/st05-select-all.png)

Que nada, isto é obra do **Database Interface**. Se entrarmos no detalhe do trace vamos notar que os dois primeiros comandos são idênticos. Ou seja, o sistema identificou que usamos **INTO CORRESPONDING FIELDS** e que a nossa tabela destino só possui alguns poucos campos e por isto converteu o OpenSQL em um comando SELECT com os campos específicos. Muito bacana não é mesmo?

Isto significa que com um pouco de cuidado é possível sim usarmos SELECT *, mas desde que a tabela de destino tenha somente os campos de interesse.