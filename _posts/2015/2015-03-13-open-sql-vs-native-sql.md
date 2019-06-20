---
title: Open SQL vs Native SQL
layout: post
lang: pt
tags: [abap]
---
O assunto que vamos explorar aqui são os comandos e os componentes envolvidos no acesso ao banco de dados na plataforma ABAP.

Normalmente quando precisamos trabalhar com um banco de dados é necessário seguir um conjunto de passos que vai desde a conexão até a execução do comando, a leitura do resultado e posteriormente garantir que estejamos fechando a conexão. Em C#, por exemplo, é necessário usar as três classes SqlConnection, SqlCommand e SqlDataReader para este trabalho, por mais simples que seja o comando. No ABAP este fluxo é drasticamente reduzido, pois não é necessário usar comandos para conectar e desconectar. Estas operações acontecem de forma implícitas na plataforma já que a configuração do banco é feita a nível de aplicação e não de código.

Veja um exemplo bem simples:

~~~
SELECT sflight~connid 
       sflight~price
  FROM sflight
  INTO TABLE gt_flight
  FOR ALL ENTRIES IN gt_scarr
  WHERE carrid = gt_scarr-carrid.
~~~

Se você conhece SQL você deve estar se perguntando, o que é aquele FOR ALL ENTRIES IN? Não existe isto em nenhum banco de dados, muito menos no padrão SQL ANSI. Acontece que o SQL do ABAP roda em uma camada superior ao banco de dados. Podemos vê-lo como uma abstração do SQL que conhecemos. 

## Open SQL

![](/public/images/2015/03/sap-db-interface.jpg)

O sistema SAP pode ser instalado em diferentes **Sistema Gerenciador de Banco de Dados**, cada qual com suas próprias particularidades quanto à sintaxe e execução de comandos SQL. Com isso faz-se necessário que possamos programar de forma independente do banco de dados, para que assim nossos programas (e o SAP em si) possa ser executado em qualquer SGBD sem que tenhamos que reescrever uma só linha de código. 

O Open SQL foi criado justamente com esta finalidade. É amplamente baseado no SQL ANSI — o que facilita bastante o aprendizado — mas inclui alguns recursos e comandos exclusivos. Em contra partida, existe também um conjunto de recursos do SQL que não são suportados.

Os comandos executados em Open SQL passam por um tradutor presente no **Database Interface**. Este tradutor é responsável por identificar em qual SGBD o SAP está instalado e fazer a conversão do comando Open SQL para o SQL que o será efetivamente executado.

Nossos comandos SQL podem ser incrementados com algumas instruções que não estão disponíveis no SGDB, como por exemplo o FOR ALL ENTRIES IN. Este comando é semelhante ao JOIN, a diferença é que o JOIN só funciona com tabelas transparentes (tabelas do banco de dados) enquanto o FOR ALL ENTRIES IN funciona somente com tabelas internas. Para mais informações de FOR ALL ENTRIES, [clique aqui](/2015/03/13/explorando-o-for-all-entries-in/ "Explorando o FOR ALL ENTRIES IN") para ler sobre que é e como funciona.

Existe ainda uma outra vantagem no uso do Open SQL relacionado ao campo **MANDT (Mandante)**. Grande parte das tabelas criadas no SAP possui um campo chamado MANDT como forma de implementar multi-tenancy. É com base neste campo que o SAP consegue separar dados de diferentes _Clients_ em uma mesma instalação do SAP. Todo comando executado no banco de dados deve utilizar este campo na hora de atualizar ou consultar registros das tabelas para evitar que um Client consiga obter informações de outro Client. Com o Open SQL não precisamos nos preocupar em nos lembrarmos de fazer isto, pois esta condição é adicionada automaticamente ao SQL.

Há mais uma vantagem no uso do Open SQL chamada de **Table Buffer**. Quando executamos um comando de consulta em uma tabela onde o buffer está ligado, a primeira ação do **Database Interface** é verificar se os dados que estamos solicitando estão no buffer (memória do Work Process).  Se não estiverem então o comando é enviado ao banco de dados e o resultado é armazenado no buffer. Isto é muito útil para tabelas que não mudam com frequência como as de parâmetros e dados mestres. Em hipótese alguma devemos ligar o buffer em tabelas de dados transacionais, pois isto representa um risco de trabalharmos com registros desatualizados. Existe também o comando **BY PASSING BUFFER** que pode ser usado quando queremos forçar a consulta no banco de dados para garantir que estamos obtendo os dados mais atualizados.

Apesar de tantas vantagens, o Open SQL não é perfeito e possui algumas limitações. Uma delas é que não é possível usar funções de coluna como SUBSTRING, TOUPPER, TOLOWER, etc.

Podem haver mais restrições, porém estas foram as que achei até o momento.

## Native SQL

O Open SQL atende a maioria dos casos onde precisamos atualizar ou coletar algum registro do banco de dados, porém caso nos deparemos com alguma limitação que nos impeça de prosseguir, é possível executar comandos nativos ao banco de dados.

Os comandos nativos não passam pela tradução do **Database Interface** e são enviados ao banco de dados da forma que nós escrevemos. Isto significa que é possível usar as funções de coluna, executar Stored Procedures, fazer todo tipo join e filtro.

Um dos motivos pela qual não se recomenda o uso de Native SQL é a dependência que este tipo de comando possui com o SGBD, isto significa que todos estes comandos devem ser revistos caso um dia o SGBD seja trocado. Além disto, perde-se alguns recursos muito importantes como Table Logging e Table Buffer.

**Importante:** Comandos nativos não adicionam o filtro de mandante automaticamente, é necessário adicioná-lo explicitamente.

Veja abaixo um exemplo de uso do Native SQL:

~~~
DATA: v_carrid TYPE scarr-carrid.

EXEC SQL PERFORMING write_output.
  SELECT carrid
  INTO   :v_carrid
  FROM   scarr
  WHERE  mandt = :sy-mandt
ENDEXEC.

FORM write_output.
  WRITE: / v_carrid.
ENDFORM.
~~~

Neste exemplo estamos coletando o resultado de um comando SQL, passando à uma variável e imprimindo-a na tela. Veja que foi usado a variável de sistema SY-MANDT para filtrar somente os registros do _Client_ atual.

## Conclusão

Agora que já conhecemos os dois métodos de acesso à dados podemos decidir onde usar um ou outro.

A recomendação da SAP é que sempre se use Open SQL. Native SQL só deve ser usado em casos onde uma restrição do Open SQL atrapalhe o desenvolvimento. Devemos evitar ao máximo atualizar registros usando Native SQL, pois como eles contornam a Database Interface, o Table Buffer não fica sabendo da atualização destes registros e isto pode levar a um sério problema de consistência.

Aproveito o post para comentar que algumas das boas práticas de SQL não se aplicam ao SAP. A ordenação de dados é um bom exemplo. Estamos acostumados a fazer instruções de ORDER BY diretamente no comando SQL. No ABAP é muito comum vermos a ordenação acontecendo na camada de aplicação, após os dados serem retornados do banco. Esta estratégia alivia a carga do banco de dados, deixado-o disponível para fazer o que ele deve fazer: armazenar e retornar dados.