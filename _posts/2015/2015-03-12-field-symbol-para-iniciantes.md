---
title: Field-Symbol para iniciantes
layout: post
comments: true
lang: pt
tags: [abap, field-symbol]
---
![](/public/images/2015/03/abap-fieldsymbol.png)

Vamos falar de Field-Symbol para finalizar a série de posts sobre variáveis.
  
Caso você ainda não tenha visto, já falamos sobre [Tipos de dados, variáveis e constantes](/2015/03/tipos-de-dados-variaveis-e-constantes/) e outro sobre [Variáveis de sistema do ABAP](/2015/03/12/variaveis-de-sistema-do-abap/).

O Field-Symbol pode ser comparado aos ponteiros na linguagem C. Uma variável declarada como Field-Symbol não armazena fisicamente um valor, ela apenas possui referência ao endereço de memória de uma variável. Toda modificação feita no símbolo é realizada na variável que está sendo referenciada.

Mas qual a utilidade disto?

Usando o Field-Symbol nós ganhamos bastante flexibilidade e desempenho na manipulação de alguns objetos. Veja abaixo um exemplo de uso.

~~~
REPORT ZFLDSYMB.

DATA: NOME(20) TYPE C VALUE 'Guilherme'.
FIELD-SYMBOLS: <FS_NOME> TYPE C.

ASSIGN ('NOME') TO <FS_NOME>.

IF <FS_NOME> IS ASSIGNED.
  <FS_NOME> = 'Joao'.
ENDIF.

WRITE <FS_NOME>.
WRITE NOME.
* Ambos os comandos imprimem 'Joao'.
* Não alteramos a variável NOME explicitamente, mas mesmo assim o 
* valor dela foi alterado por conta da atribuição realizada no ponteiro.
~~~

Na **linha 4** estamos definindo a nossa Field-Symbol do tipo C. Note que nesta declaração começamos com a instrução **FIELD-SYMBOLS** seguido do nome da símbolo iniciando com **<** e terminando com **>**. Esta padrão é obrigatório e muito útil, pois nos ajuda a distinguir um Field-Symbol de uma Work Area.

Na **linha 6** estamos atribuindo o endereço da variável **NOME** ao símbolo **<FS_NOME>**. A partir de agora toda operação realizada no símbolo será replicada para a variável nome e vice-versa. Isto é válido até que a referencia seja desfeita usando a instrução **UNASSIGN <FS_NOME>**.

Na **linha 8** estamos fazendo uma operação extremamente importante quando o assunto é Field-Symbols. É necessário tomar muito cuidado na hora de manipular este tipo de objeto, pois caso você tente usar um símbolo que ainda não foi atribuído à um endereço é esperado que ocorra uma DUMP no SAP (uma mensagem de erro seguido do fechamento forçado da transação corrente). Nesta linha estou executando WRITE no símbolo somente se este símbolo está **ASSIGNED**.

Este foi um exemplo bem simples, mas já deu para entender como funciona. Certo?
  
Agora volte ao exemplo e note também que durante a atribuição do símbolo o nome da variável aparece entre aspas. Sabe o que isto significa? Isto é o tal do dinamismo que esta técnica proporciona.

Veja este novo exemplo.

~~~
REPORT ZFLDSYMB.

DATA: NOME(20) TYPE C VALUE 'Guilherme',
      SOBRENOME(20) TYPE C VALUE 'Oenning',
      VAR(10) TYPE C,
      CONT TYPE I.

FIELD-SYMBOLS: <FS_OBJ> TYPE ANY.

IF CONT = 0.
  VAR = 'NOME'.
ELSE.
  VAR = 'SOBRENOME'.
ENDIF.

ASSIGN (VAR) TO <FS_OBJ>.

IF <FS_OBJ> IS ASSIGNED.
  <FS_OBJ> = 'Aloha'.
ENDIF.

WRITE: NOME, / SOBRENOME.
~~~

Veja a **linha 16**. Estamos criando a referência do símbolo para uma variável que só vamos saber durante a execução do programa. Também é possível atribuir dinamicamente um campo de uma estrutura usando o comando **ASSIGN COMPONENT**. A vantagem desta abordagem é que não são necessárias tantas declarações de variáveis, consequentemente o programa usará menos memória. Tome cuidado para não tentar atribuir uma variável que não existe, pois o verificador de sintaxe **NÃO** tem como nos alertar disto.

~~~
DATA: NOME(20) TYPE C VALUE 'Guilherme'.
FIELD-SYMBOLS: <FS_NOME> TYPE C.
ASSIGN ('MEU_NOME') TO <FS_NOME>.
WRITE <FS_NOME>. "DUMP!!!
~~~

Um outro exemplo muito usado é na modificação de registros de uma tabela interna. Da forma tradicional nós precisamos definir uma work area para a iteração e posteriormente executar um MODIFY na tabela interna. Como o Field-Symbol aponta diretamente para a área de memória onde a linha da tabela está armazenada, no momento em que alterarmos uma propriedade do símbolo a alteração já estará sendo efetivada na tabela interna. Além de necessitar menos linhas de código ainda poupamos alguns bytes de memória por não estar mais alocando espaço de memória na criação da work area.

Veja abaixo um exemplo da forma tradicional e outro usando Field-Symbol.

~~~
REPORT ZIT_TRADICIONAL.

TYPES:
  BEGIN OF t_nome,
    primeiro(20) TYPE c,
    ultimo(20) TYPE c,
  END OF t_nome,
  BEGIN OF t_pessoa,
    codigo TYPE i,
    nome TYPE t_nome,
    idade TYPE i,
    peso TYPE p DECIMALS 2,
  END OF t_pessoa.

DATA: alunos TYPE STANDARD TABLE OF t_pessoa,
      wa_aluno TYPE t_pessoa.

wa_aluno-codigo = 1.
wa_aluno-nome-primeiro = 'Albert'.
wa_aluno-nome-ultimo = 'Einstein'.
wa_aluno-idade = 42.
wa_aluno-peso = '70.2'.
INSERT wa_aluno INTO TABLE alunos.

wa_aluno-codigo = 2.
wa_aluno-nome-primeiro = 'Thomas'.
wa_aluno-nome-ultimo = 'Edison'.
wa_aluno-idade = 32.
wa_aluno-peso = '85.2'.
INSERT wa_aluno INTO TABLE alunos.

LOOP AT alunos INTO wa_aluno.
  IF wa_aluno-codigo = 1.
    wa_aluno-idade = 30.
    MODIFY alunos FROM wa_aluno INDEX sy-tabix.
  endif.
ENDLOOP.

LOOP AT alunos INTO wa_aluno.
 WRITE: / wa_aluno-nome-primeiro, wa_aluno-nome-ultimo, wa_aluno-idade.
ENDLOOP.
~~~

~~~
REPORT ZIT_FLDSYMB.

TYPES:
  BEGIN OF t_nome,
    primeiro(20) TYPE c,
    ultimo(20) TYPE c,
  END OF t_nome,
  BEGIN OF t_pessoa,
    codigo TYPE i,
    nome TYPE t_nome,
    idade TYPE i,
    peso TYPE p DECIMALS 2,
  END OF t_pessoa.

FIELD-SYMBOLS: <fs_aluno> type t_pessoa.

DATA: alunos TYPE STANDARD TABLE OF t_pessoa.

INSERT INITIAL LINE INTO TABLE alunos ASSIGNING <fs_aluno>.
<fs_aluno>-codigo = 1.
<fs_aluno>-nome-primeiro = 'Albert'.
<fs_aluno>-nome-ultimo = 'Einstein'.
<fs_aluno>-idade = 42.
<fs_aluno>-peso = '70.2'.

INSERT INITIAL LINE INTO TABLE alunos ASSIGNING <fs_aluno>.
<fs_aluno>-codigo = 2.
<fs_aluno>-nome-primeiro = 'Thomas'.
<fs_aluno>-nome-ultimo = 'Edison'.
<fs_aluno>-idade = 32.
<fs_aluno>-peso = '85.2'.

LOOP AT alunos ASSIGNING <fs_aluno>.
  IF <fs_aluno>-codigo = 1.
    <fs_aluno>-idade = 30.
  endif.
ENDLOOP.

LOOP AT alunos ASSIGNING <fs_aluno>.
 WRITE: / <fs_aluno>-nome-primeiro, <fs_aluno>-nome-ultimo, <fs_aluno>-idade.
ENDLOOP.
~~~

Na **linha 15** estou **definindo um Field-Symbol do tipo t_pessoa**. Isto significa que este só pode referenciar objetos/variáveis do tipo **t_pessoa**.
  
Na **linha 19** estou criando um novo registro na tabela alunos e já referenciando meu símbolo para este registro recém criado. Neste momento já posso utilizá-lo como se fosse um objeto do tipo t_pessoa populando todos os campos necessários. Esta operação é repetida na **linha 26** para criar um segundo registro.
  
Na **linha 33** estou fazendo um LOOP em todos os registros da minha tabela interna e apontando cada uma das linhas da iteração para o meu símbolo. Na **linha 34** eu verifico se o objeto em questão é o que eu preciso alterar (usando a condicional IF) e então na **linha 35** eu efetivo a modificação do objeto. Veja que diferente do exemplo tradicional, eu não preciso mais do comando **MODIFY** para concluir a minha alteração. Isto porque o símbolo já está referenciando diretamente o objeto em memória, enquanto que no exemplo anterior a **work area era apenas uma cópia do objeto**.

Isto foi tudo o que aprendi nos meus estudos de Field-Symbol. Espero que este resumo possa ser útil para vocês!