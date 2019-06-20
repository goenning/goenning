---
title: Tipos de dados, variáveis e constantes
layout: post
lang: pt
tags: [abap]
---
Conhecer o fundamento dos tipos de dados e suas opções é essencial para podermos construir programas de qualidade, principalmente a partir da versão 4.5 onde a linguagem passou a suportar os modelos de programação estruturado e orientado a objetos de forma mista. Parece besteira, mas o funcionamento dos tipos de dados em ABAP possui uma diferença considerável quando comparada com outras linguagens.

A imagem abaixo ajuda a enxergarmos todas as possibilidades.

![](/public/images/2015/03/abap-data-types.gif)

Note que a imagem acima quebra os tipos de dados em três categorias: **Elementary Types**, **Complex Types** e **Reference Types**. Vamos dar uma olhada em cada um deles e suas diferenças.

## Elementary Types

Também conhecidos como tipos primitivos, estes são os famosos tipos de dados que encontramos em todas as linguagens. Temos o Inteiro, o Caractere, Data e Hora como bons exemplos.

A definição de variáveis é feita com o comando DATA conforme mostra o exemplo abaixo.

~~~
REPORT zvariaveis.
DATA: cep(8) TYPE n,
      nome(20) TYPE c,
      idade TYPE i.
~~~

Vale a pena ressaltar a diferença entre C (CHAR) e N (NUMC) que geralmente causa bastante confusão. N possui o mesmo comportamento do tipo C com a única diferença que este só pode armazenar valores numéricos. Ele ocupa a mesma quantidade de bytes em memória e é sempre representado com zeros à esquerda até preencher o comprimento total da variável. Também é possível utilizarmos variáveis deste tipo em operações matemáticas como adição, subtração e até comparação maior ou menor. 

No início pode parecer um tanto quanto estranho usar o tipo N se temos o I, mas ao longo do tempo você perceberá que a SAP faz uso extensivo deste de NUNC e logo você estará fazendo isto também. Nas chaves primárias de tabelas, por exemplo, sempre será utilizado NUMC. Um dos motivos para isto é justamente a garantia do tamanho fixo que este tipo de dado possui, pois em muitos lugares o SAP armazena a chave primária das tabelas de forma concatenada. Nestes casos é obrigatório que os campos da chave tenham um tamanho fixo para que seja possível fazer as devidas conversão na concatenaçao.

Há ainda os tipos de dados de tamanho variável: **STRING** e **XSTRING**. Diferente de CHAR, o STRING pode crescer o quanto for necessário, sendo então o tipo de dado ideal para armazenar uma grande quantidade de texto como o conteúdo de um arquivo ou de um documento XML, por exemplo. 

## Complex Types

No ABAP também é possível criarmos nossas próprias estruturas de dados. Isto é muito útil quando é necessário agrupar um conjunto de informações em uma única variável.
  
São dois passos para usar estas estruturas. A primeira é criar a estrutura usando a instrução TYPES. A segunda é criar uma instância deste tipo da mesma forma que fizemos até agora, usando a instrução DATA.

~~~
REPORT  zvariaveis.
TYPES: BEGIN OF t_pessoa,
    nome(20) TYPE c,
    idade TYPE i,
    peso TYPE p DECIMALS 2,
  END OF t_pessoa.

DATA: homem TYPE t_pessoa,
      mulher TYPE t_pessoa.

homem-nome = "Joao".
homem-idade = 22.
homem-peso = '72.51'.

mulher-nome = "Maria".
mulher-idade = 19.
mulher-peso = '68.2'.
~~~

Para quem já trabalhou com outras linguagens, note que no ABAP o acesso aos campos é feito com hífen ao invés de ponto, isto causa bastante confusão em quem está inciando. O &#8216;ponto&#8217; sempre será usado para definir o final de uma linha de código.

Também é possível criarmos uma estrutura de dados que se comporta como uma hierarquia, onde o tipo de um campo referencia outro tipo complexo. Ahm?! Não entendeu? O exemplo abaixo deve ajudar na explicação. Transformei o campo nome em outra estrutura de dados.

~~~
REPORT  zvariaveis.
TYPES:
  BEGIN OF t_nome,
    primeiro(20) TYPE c,
    ultimo(20) TYPE c,
  END OF t_nome,
  BEGIN OF t_pessoa,
    nome TYPE t_nome,
    idade TYPE i,
    peso TYPE p DECIMALS 2,
  END OF t_pessoa.

DATA: homem TYPE t_pessoa,
      mulher TYPE t_pessoa.

homem-nome-primeiro = 'Joao'.
homem-nome-ultimo = 'Braga'.
~~~

Ainda em se tratando de tipos complexos temos também as **Tabelas Internas**. A tabela interno é possivelmente um dos tipos de variável mais usado em programas ABAP, então se este assunto for novidade para você, sugiro estudá-lo e praticá-lo bastante. 

Uma tabela interno é muito semelhante a uma tabela de banco de dados. A grande diferença é que ela nasce e morre dentro de um contexto de um programa, ou seja, os dados inseridos nesta tabela não são armazenados em disco. Poderíamos chamá-las também de tabela temporária, porém este termo não é usado no ambiente ABAP.

Quando trabalhamos com uma tabela interna é necessário declararmos uma variável do tipo tabela referenciando um outro tipo de dados, podendo ser um Elementary Type ou Complex Type. O tipo referenciado em uma tabela é conhecido como **LINE TYPE**. Veja o seguinte exemplo de declaração.

~~~
REPORT  zvariaveis.
TYPES:
  BEGIN OF t_nome,
    primeiro(20) TYPE c,
    ultimo(20) TYPE c,
  END OF t_nome,
  BEGIN OF t_pessoa,
    nome TYPE t_nome,
    idade TYPE i,
    peso TYPE p DECIMALS 2,
  END OF t_pessoa.

DATA: alunos TYPE STANDARD TABLE OF t_pessoa.
~~~

Neste exemplo criei uma tabela chamada alunos onde o tipo da linha é t_pessoa.
  
Esta variável/tabela pode conter uma quantidade infinita de registros, portanto torna-se útil quando não sabemos a quantidade de registros que precisamos. Se tivermos certeza de que será sempre dois registros, poderíamos criar duas variáveis básicas do tipo t_pessoa. No caso de um relatório de alunos matriculados em um curso, pode ser que tenhamos 1, 2, 3, 10, 40, 50 ou até mais alunos. Seria inviável criar uma variável para armazenar os dados de cada aluno, então este é um cenário onde devemos usar variáveis do tipo tabela.

Comparando com outras linguagens, isto seria o mesmo que instanciar um objeto ArrayList (Java) ou List (C#) de uma classe qualquer.

Para poder inserir registros nestas tabelas é necessário criar o que o ABAP chama de **work area**. Work area é uma variável do mesmo tipo de dado da tabela, porém representa apenas uma linha da tabela. Toda vez que for necessário inserir ou manipular um registro da tabela, é necessário fazer estas modificações primeiro na work area e depois repassar para a tabela. Confuso? Vamos ver se consigo explicar melhor com um exemplo.

~~~
REPORT zvariaveis.

TYPES:
  BEGIN OF t_nome,
    primeiro(20) TYPE c,
    ultimo(20) TYPE c,
  END OF t_nome,
  BEGIN OF t_pessoa,
    codigo type i,
    nome TYPE t_nome,
    idade TYPE i,
    peso TYPE p DECIMALS 2,
  END OF t_pessoa.

* definição da tabela interna de alunos
DATA: alunos TYPE STANDARD TABLE OF t_pessoa,
* definição da work area para a tabela alunos
      wa_aluno type t_pessoa.

* inserindo o primeiro registro de aluno na tabela de alunos
wa_aluno-codigo = 1.
wa_aluno-nome-primeiro = 'Albert'.
wa_aluno-nome-ultimo = 'Einstein'.
wa_aluno-idade = 42.
wa_aluno-peso = '70.2'.
insert wa_aluno into table alunos.

* inserindo o segundo registro de aluno na tabela de alunos
wa_aluno-codigo = 2.
wa_aluno-nome-primeiro = 'Thomas'.
wa_aluno-nome-ultimo = 'Edison'.
wa_aluno-idade = 32.
wa_aluno-peso = '85.2'.
insert wa_aluno into table alunos.

* mostrando na tela o nome completo e a idade dos dois alunos.
LOOP AT alunos INTO wa_aluno.
 write: / wa_aluno-nome-primeiro, wa_aluno-nome-ultimo, wa_aluno-idade.
ENDLOOP.

* seleciona o aluno onde codigo for igual a 1 e o posiciona na work area
READ table alunos INTO wa_aluno WITH KEY codigo = 1.
IF sy-subrc = 0.
* modifica a idade do aluno
  wa_aluno-idade = 30.
* atualiza a tabela de alunos usando o indice encontrado na busca  
  MODIFY alunos FROM wa_aluno INDEX sy-tabix.
ENDIF.

* mostrando na tela o nome completo dos dois alunos, com a idade atualizada
LOOP AT alunos INTO wa_aluno.
 write: / wa_aluno-nome-primeiro, wa_aluno-nome-ultimo, wa_aluno-idade.
ENDLOOP.
~~~

Nestes exemplos eu sempre usei a tabela do tipo STANDARD. Mas também é possível usar HASHED ou SORTED. A diferença está na ordem com que os registros são inseridos e no desempenho durante os comandos de LOOP E READ TABLE. O artigo [Performance de leitura por tipo de tabela](/2015/03/18/performance-de-leitura-por-tipo-de-tabela/ "Performance de leitura por tipo de tabela") explora bastante este assunto. 

#### Diferente formas de declarar uma variável.

Em muitos exemplos de código na internet é possível notar o uso do comando LIKE na definição de uma variável. A diferença entre o TYPE e LIKE é que no TYPE você deve informar qual o tipo de dado da variável, eunquanto a declaração usando LIKE você informa uma outra variável existente para que esta nova variável copie a definição do tipo de dado. Este comando tornou-se obsoleto, pois não funciona com a versão orientada a objeto do ABAP. A SAP recomenda o uso de TYPE para todos os casos de declaração de variáveis. 

Também é muito comum encontrar exemplos onde a tabela interna é definida com HEADER LINE. Header line é uma forma de não ter que declarar uma work area para trabalhar com tabelas. Com o passar do tempo o uso de header line caiu em desuso e, apesar de ainda funcionar, não é mais recomendado.

## Reference Types

Variáveis de referência foram adicionadas ao ABAP junto com o início do suporte à programação orientada à objetos. Referências são ponteiros que funcionam tanto para objetos quanto para os tipos de dados apresentados acima. Quando declaramos uma variável de referência seu conteúdo permanece vazio até que este seja criado com a instrução **CREATE OBJECT** ou **CREATE DATA**, ou então seja atribuída à uma variável que já existe com a instrução **GET REFERENCE OF&#8230;INTO**. A diferença entre CREATE DATA e CREATE OBJECT é que o primeiro é usado para criar referências à tipos de dados primários ou complexos, enquanto o segundo só funciona com classes ABAP.

Segue abaixo um exemplo bem simples usando referências com um tipo de dado inteiro.

~~~
DATA: lv_ref   TYPE REF TO i,
      lv_value TYPE i.

lv_value = 10.
GET REFERENCE OF lv_value INTO lv_ref.
WRITE lv_value. 

lv_ref-&gt;* = 12.

WRITE lv_value.
~~~

Neste exemplo primeiro é impresso 10 e depois 12. Ou seja, o conteúdo da variável LV_VALUE foi alterado implicitamente, pois foi alteado pela variável de referência que estava apontando para ela. 

Este tipo de cenário é pouco explorado no ABAP, normalmente o uso mais visto para variáveis de referência é quando precisamos trabalhar com classes e objetos. Quando precisamos instanciar uma classe em ABAP fazemos o uso de CREATE OBJECT. Um exemplo bem familiar para muitos programadores ABAP é a classe **CL\_GUI\_ALV_GRID** que representa um elemento gráfico de um ALV (ABAP List Viewer, ou apenas Grid).

~~~
DATA : lo_alv TYPE REF TO cl_gui_alv_grid.
 
CREATE OBJECT lo_alv
  EXPORTING
    i_parent = lo_my_container.
~~~

Neste cenário estamos criando uma instância em tempo de execução da classe CL\_GUI\_ALV_GRID e atribuindo-a à nossa variável de referência. É a partir deste momento que podemos fazer uso de seus métodos e propriedades sem resultado em um DUMP. O DUMP neste caso seria igual ao NullPointerException ou NullReferenceException do Java e C#, conhecem?

## Bônus: Constantes

Não vou me estender muito neste assunto. Vou partir do princípio que você já sabe o que é uma constante e vou direto ao código que é o que interessa.

~~~
CONSTANTS c_empresa(4) TYPE n VALUE 1001.
~~~

Também é possível criar uma estrutura constante. Ficaria assim:

~~~
CONSTANTS: BEGIN OF c_empresa,
            codigo(4) TYPE n VALUE '1001',
            nome TYPE c VALUE 'SAP'
           END OF c_empresa.

WRITE: / c_empresa-codigo, c_empresa-nome.
~~~

Espero que este post tenha sido útil.
  
Até a próxima!