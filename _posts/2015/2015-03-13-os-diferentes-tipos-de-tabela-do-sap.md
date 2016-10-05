---
title: Os diferentes tipos de tabela do SAP
layout: post
comments: true
lang: pt
tags: [abap, cluster table, internal table, pooled table, sap, sap tables, transparent table]
---

No SAP existem pelo menos quatro tipos diferentes de tabela, cada qual com suas características, vantagens, restrições e modo de uso. Neste post vamos estudar um pouco sobre ada uma delas. Primeiro vamos conhecer quais são. Temos a **Internal Table**, **Transparent Table**, **Pooled Table** e a **Cluster Table**.

# Internal Table

Vamos iniciar falando deste, pois possui um comportamento que difere de todas as outras três.
  
Este é o único tipo de tabela que não é compartilhado entre processos e também não é persistente, ou seja, os dados são armazenados na memória e uma vez finalizado o programa os dados da tabela são perdidos. Tabelas internas são usadas em programas para armazenar dados temporários que serão usados para algum cálculo ou algum outro tipo de processamento. Escrevi um pouco sobre como usar tabelas internas [neste post](/2015/03/12/variaveis-de-sistema-do-abap/), então aqui só vou complementar a explicação com os possíveis tipos de tabela interna (quase um inception de tipos de tabela ein?!)

### Standard Table

Neste tipo de tabela os registros são armazenados na ordem que foram inseridos na tabela. É usado quando não nos importamos com a ordem dos registros e quando podemos usar o índice do registro na tabela para ler e manipular seus dados. Como isto não é muito comum, usa-se bastante a combinação dos comandos SORT e BINARY SEARCH. Desta forma o desempenho será semelhante ao de uma Sorted Table, pois caso contrário o ABAP terá que fazer um table scan para encontrar os registros.

~~~
* Exemplo usando INDEX
DATA: it_clientes TYPE STANDARD TABLE OF ty_cliente.
READ TABLE it_clientes INDEX 1 ASSIGNING <fs_cliente>.
DELETE it_clientes INDEX 5.

* Exemplo usando BINARY SEARCH
DATA: it_clientes TYPE STANDARD TABLE OF ty_cliente.
SORT TABLE it_clientes by codigo.
READ TABLE it_clientes WITH KEY codigo = 16 ASSIGNING <fs_cliente> BINARY SEARCH.
~~~

### Sorted Table

Quando criamos uma Sorted Table precisamos necessariamente informar uma chave, pois é através dela que o ABAP fará a ordenação dos registros inseridos na tabela. A diferença desta é que a chave pode ser única ou não-única, enquanto a Standard Table aceita somente chaves não-única. Vejamos abaixo como fica o uso. Notem que neste caso não é mais necessário ordenar manualmente pois a tabela já está sempre ordenada. Também não é necessário usar BINARY SEARCH, pois este já é usado implicitamente.

~~~
DATA: it_clientes TYPE SORTED TABLE OF ty_cliente WITH UNIQUE KEY codigo.
READ TABLE it_clientes WITH KEY codigo = 16 ASSIGNING <fs_cliente>.
~~~

### Hashed Table

Já neste tipo de tabela é obrigatório termos uma chave única, pois é através dela que será aplicada uma função hash para que o ABAP possa ir diretamente no registro sem ter que ficar fazendo buscas como no caso dos outros dois tipos de tabelas. Em termos de performance é o mesmo que usarmos INDEX da Standard Table. A diferença é que é muito mais fácil e natural trabalhar com chaves de tabela do que índices de registro, não é mesmo?

~~~
DATA: it_clientes TYPE HASHED TABLE OF ty_cliente WITH UNIQUE KEY codigo.
READ TABLE it_clientes WITH KEY codigo = 16 ASSIGNING <fs_cliente>
~~~

Esta imagem ajuda a resumir bem a diferença entre elas. 

![](/public/images/2015/03/abap_pts3.jpg)

O desempenho das três tabelas é bastante semelhante quando definimos uma chave e a usamos nas buscas. Por este e outros motivos é que a Standard Table é hoje o tipo de tabela mais usada. É ersátil e conseguimos obter desempenho equivalente. [Neste artigo](/2015/03/18/performance-de-leitura-por-tipo-de-tabela/ "Performance de leitura por tipo de tabela") você encontra uma análise de quando usar cada tipo de tabela do ponto de vista de desempenho.

![](/public/images/2015/03/zgdc_inttab_perf_test_graph.png)

Agora que já falamos do arroz com feijão, vamos seguir para os verdadeiros tipos de tabela.

# Transparent Table

Este é o tipo de tabela mais conhecido, mais usado e também o mais fácil de entender. Isto porque seu funcionamento é idêntico a qualquer outra tabela de um banco de dados relacional. Para cada Transparent Table definida na SE11 (Data Dictionary) uma tabela física é criada no banco de dados com exatamente o mesmo nome, os mesmos campos e os mesmos índices. Vamos dar uma olhada em um exemplo prático.

Criei esta tabela Z na transação SE11. Reparem no nome da tabela e nos campos.

![](/public/images/2015/03/transparent-table-zemployees-se11.png)

Agora vejam como o SAP criou esta tabela no banco de dados. No meu exemplo estou usando o banco MaxDB, mas o funcionamento é o mesmo para qualquer outro banco.

![](/public/images/2015/03/transparent-table-zemployees-maxdb.png)

Vejam também como ficam os registros criados nesta tabela.

![](/public/images/2015/03/transparent-table-zemployees-records.png)

Esta é o tipo de tabela padrão quando usamos a SE11 justamente por ser usada em praticamente 99% dos casos quando comparado com a Pooled Table e Cluster Table.
  
Para consultar e manipular os dados destas tabelas é possível usar tanto [OpenSQL quanto NativeSQL](/2015/03/13/open-sql-vs-native-sql/).

# Pooled Table

Assim como a Transparent Table, este tipo de tabela também possui uma tabela física criada no banco, ou seja, também é persistente. A diferença é que neste modelo diversas Pooled Table **compartilham a mesma tabela física**. E como isto funciona? Pois bem, quando criamos uma Pooled Table na SE11 precisamos informar qual a Table Pool (Tabela Física) que nossa tabela lógica vai usar. Fiz um exemplo onde criei um Pool chamado **ZPOOL** e duas Pooled Tables, **ZPAISES** e **ZCIDADES**

Vejamos primeiro como ficaram as tabelas no Data Dictionary.

![](/public/images/2015/03/pooled-table-se11.png)

Notem que na aba **Delivery and Maintenance** eu coloquei a Pool Table **ZPOOL**.
  
As tabelas ZCIDADES e ZPAISES não existem no banco de dados, mas a tabela ZPOOL existe. A estrutura desta tabela é genérica e aceita registros de diferentes formatos e estruturas.
  
Vejam só como ficaram os registros destas tabelas no banco de dados.

![](/public/images/2015/03/pooled-table-zpool-maxdb.png)

**TABNAME** armazena o nome da tabela que criamos no dicionário. **VARKEY** é a concatenação de todos os campos da chave primária. **DATALN** segundo a documentação é o comprimento dos dados (no meu exemplo não parece ser ou eu não entendi). **VARDATA** é a concatencação de todos os campos de dados da tabela em formato binário.

Devido ao comportamente exótico deste tipo de tabela algumas restrições se aplicam. A primeira é que não é possível usar NativeSQL nas Pooled Tables justamente por elas não existirem no banco de dados. A segunda é que todos os campos da chave primária devem ser do tipo caractere (NUMC, CHAR ou CLNT). A terceira é que é não é possível criar índices secundários. A quarta é que as funções de agregação (SUM, AVG, MIN, MAX, etc) também não são suportadas. E por fim também não é possível usar instruções de JOIN no OpenSQL. Quanta desvantagem, ein?

Este tipo de tabela é bastante usado pelo SAP para tabela de parâmetros de sistema e customizing, portanto é muito raro termos que criar uma tabela deste tipo. Normalmente estas tabelas são &#8220;bufferizadas&#8221;, pois não é comum que estes registros sejam alterados. Outra característica é que **SELECT *** ou **SELECT campo1, campo2** não faz a menor diferença, pois o SAP vai precisar retornar todas as colunas de qualquer forma.

![](/public/images/2015/03/sap-pooled-table.gif)

# Cluster Table

As Cluster Tables possuem uma certa semelhança com a Pooled Table. Isto porque quando criamos uma tabela deste tipo precisamos informar qual será a tabela do banco de dados que irá armazenar os dados fisicamente. Várias Cluster Tables compartilham a mesma tabela física. Igual ao Pooled Table, correto? Até aqui sim, a grande diferença é que quando criamos um cluster de tabelas devemos informar qual a chave primária da tabela e todas as tabelas lógicas que compõem este cluster precisam conter a mesma chave. Isto é necessário pela forma com que o SAP armazena estes registros na tabela física. Vamos ver um exemplo disto na prática.

Vamos tomar como exemplo um sistema de Recursos Humanos. Na ficha cadastral de um funcionário existem diversas informações que são separadas por categoria. Ex.: dados básicos, dados de endereço, dados profissionais, dados familiares, dados de veículos e etc.
  
O que vamos ver agora é um cluster de tabela de funcionários. Vou criar 3 tabelas dentro deste cluster. São elas: Dados de Cabeçalho, Dados Básicos e Dados de Veículos.

Na SE11 criei o cluster ZFUNC conforme imagem abaixo.

![](/public/images/2015/03/table-cluster-zfunc-se11.png)

Notem que as chaves deixam de ser genéricas como na Pooled table e passam a ter dados inerentes ao negócio. Em verde destaquei os campos que são obrigatórios para o SAP gerenciar este tipo de tabela.

E aqui estão as minhas 3 tabelas criadas na SE11 e que são do tipo Cluster Table. 

![](/public/images/2015/03/table-cluster-zfunc-se111.png)

Vejam que todas elas iniciam com a mesma chave primária do meu cluster. A tabela de veículos possui um campo a mais na chave, tornando-a uma tabela de um-para-muitos em relação à tabela de cabeçalho.

Após criados todas estas tabelas, se olharmos no banco de dados vamos notar que existe apenas uma tabela chamada de ZFUNC. Não existe nada referente à ZFUNC\_C, ZFUNC\_B ou ZFUNC_V.
  
E vejam só como os registros são organizados dentro da tabela física.

![](/public/images/2015/03/table-cluster-zfunc-maxdb.png)

Criei 3 funcionários e para um deles criei 2 registros de veículos. Mesmo assim a tabela se manteve com apenas uma linha para cada registro de cabeçalho. Todos os outros dados que não compõem a chave primária do cluster são armazenados dentro do campo VARDATA. Caso este campo ultrapasse o tamanho máximo, uma nova linha é criada e o campo PAGENO passa a ser 1, depois 2, depois 3 e assim por diante.

Assim como na Pooled Table, índices secundários não são suportados assim como Joins também não. NativeSQL e funções de agregação também não funcionam. SELECT * está liberado (boa notícia para os preguiçosos de plantão). Já para as chaves primárias não temos mais a restrição de usar somente tipos de dados caracteres.
  
Uma característica importante deste tipo de tabela é que como todos os registros de uma mesma chave estão em um único registro da tabela, quando fazemos uma consulta em qualquer uma das tabelas lógicas de um cluster, o SAP já carrega no buffer os dados de todas as outras tabelas lógicas para aquela mesma chave primária.

Fiz o seguinte programa de exemplo.

~~~
data: wa_func type zfunc_c,
      gt_veic type table of zfunc_v.

field-symbols: <fs_veic> type zfunc_v.

select single * from zfunc_c into wa_func where empresa = 1000 and matricula = 1.
if sy-subrc is initial.
  write wa_func-nome.
  select * from zfunc_v into table gt_veic where empresa = wa_func-empresa and matricula = wa_func-matricula.
    if sy-subrc is initial.
      loop at gt_veic ASSIGNING <fs_veic>.
        write: / '-' , <fs_veic>-placa, <fs_veic>-modelo.
      endloop.
    endif.
endif.
~~~

Vejam que este programa faz duas consultas no banco de dados. Uma para trazer o cabeçalho e outra para trazer os veículos. Acontece que se usarmos a ST05 para analisar o trace deste programa, é possível confirmar que o SAP realmente só faz uma única consulta na tabela física. Veja só o trace gerado em meu ambiente.

![](/public/images/2015/03/select-cluster-table.png)

Bacana, não é? É um recurso que pode ser útil mas ao mesmo tempo perigoso. Por isto este tipo de tabela não é muito usado no SAP Standard e muito menos nas customizações que fazemos.

![](/public/images/2015/03/sap-cluster-table.gif)

# Conclusão

Considero sempre importante conhecer os detalhes por trás de cada recurso quando trabalhamos com uma tecnologia nova para termos a capacidade de optar pela solução que melhor nos atende. Com o que tenho lido e aprendido sobre cada um dos tipos de tabela, posso dizer que não vejo nenhum caso onde eu usaria os tipos Pooled e Cluster. Talvez na construção de um grande módulo Z complementar ao SAP poderia haver um cenário de uso para Pooled Table, mas para o dia a dia prefiro continuar apemas com as Transparent Table. [Neste post do SCN](http://scn.sap.com/thread/1291370) um participante comenta que na instalação do SAP dele existem 69984 transparent tables, 2019 pooled tables e apenas 77 clustered tables. O ambiente dele pode variar para o nosso, mas já da para ter uma noção da proporção de uso de cada uma delas.

Espero que a explicação tenha sido útil de alguma forma.

Abraço!