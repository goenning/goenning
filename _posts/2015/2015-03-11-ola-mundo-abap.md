---
title: Olá Mundo ABAP
layout: post
lang: pt
tags: [abap, hello world, se80]
---

Hello World é provavelmente o programa mais conhecido no mundo da programação. É o programa mais simples de se escrever e por isto é constantemente usado para dar o ponta pé inicial nos estudos de ma nova linguagem. Aqui não vai ser diferente, vamos fazer o famoso programa em ABAP.

Mas antes de escrevermos qualquer linha de código, gostaria de explicar rapidamente sobre o workbench.

O workbench é um ambiente integrado de programação dentro do próprio sistema SAP. Trata-se de um conjunto de transações que são usadas durante o desenvolvimento de programas em ABAP.

A transação mais comum é a **SE80**, conhecido como **Object Navigator**. Nele é possível criarmos programas, telas, includes, menu e outros componentes de desenvolvimento.

Outra bastante famosa é a SE38 (ABAP Editor). A diferença entre estas duas transações é que este última é exclusiva para edição de código fonte ABAP, como se fosse um notepad com plugins de sintaxe de ABAP. Já a SE80 seria comparado à um eclipse, onde você tem organizado por pacotes/pastas todos os componentes dos seus programas.

![](/public/images/2015/03/sap-se80.png)

Vamos começar nosso exemplo executando a transação SE80 e criando nosso primeiro programa.

Ao entrar nesta transação, escolha a opção **Repository Browser** e então estará disponível uma pasta que começa com $TMP_<seu-usuario>. Caso a pasta não apareça, clique no primeiro combo box abaixo do Repository Browser e escolha Local Objects.

Esta pasta é na verdade chamada de pacote. Estes pacotes servem para agrupar um conjunto de objetos do repositório (tabelas, programas, classes, etc.). O correto seria escolher um pacote de verdade se estivéssemos em um ambiente real de SAP, pois objetos que estão no pacote temporário $TMP não podem ser transportados para os ambiente de qualidade e produção.

Vamos criar um novo programa dentro da subpasta Programs. Para isto clique com o botão direito na pasta e depois em create.

Estou usando o nome ZHELLO\_WORLD para o programa e com a opção de &#8220;With TOP include&#8221; habilitada. Para o nome da include TOP estou usando o padrão <nome\_programa>_TOP.

**Importante: **Os programas ABAP possuem alguns padrão recomendados pela SAP. Um deles é que toda definição de variáveis e estruturas de dados globais devem ficar em um arquivo de include conhecido como TOP.

Os atributos ficaram assim:

![](/public/images/2015/03/zhelloworld-1.png)

Este programa que estamos criando é do tipo &#8220;Executable program&#8221;, conhecido também como Report. Um programa que possui uma tela com alguns parâmetros de entrada e uma tela de saída de dados.

Já com o editor aberto, podemos começar nossa codificação. A estrutura de pastas deve ter ficado como esta da figura abaixo.

![](/public/images/2015/03/zhelloworld-2.png)

Limpe o arquivo Z\_HELLO\_WORLD\_TOP e inclua o seguinte código no arquivo ZHELLO\_WORLD.

~~~
INCLUDE zhello_world_top.
REPORT zhello_world.
WRITE 'Olá Mundo'.
~~~

O comando REPORT marca o início do programa enquanto o WRITE &#8216;Olá Mundo&#8217; escreve na tela. Para executar basta apertar F8.

Parabéns! Você criou e executou seu primeiro programa em ABAP.

Vamos agora para um exemplo com parâmetros de entrada.

~~~
REPORT zhello_world.
INCLUDE zhello_world_top.

PARAMETERS: p_nome(20) TYPE c.

WRITE: 'Olá ', p_nome.
~~~

Foi criado um parâmetro de entrada chamado **p_nome** do tipo caractere e de no máximo 20 posições. O comando WRITE também foi modificado para receber desta vez duas strings.
  
Note que após o comando write foi incluído o caractere &#8220;:&#8221;. Quando for necessário executar várias vezes o mesmo comando em ABAP você pode adicionar &#8220;:&#8221; após o comando e em seguida passar os parâmetros separados por uma vírtula &#8220;,&#8221;. Desta forma o ABAP irá executar este comando uma vez para cada parâmetro. Ou seja, o WRITE acima tem o mesmo resultado que o exemplo abaixo.

~~~
WRITE 'Olá'.
WRITE p_nome.
~~~

Criamos o arquivo de include mas ainda não o utilizamos. Vamos definir neste arquivo uma variável caractere para receber a concatenação de &#8216;Olá&#8217; e o nome digitado.

~~~
DATA: c_saudacao(40) TYPE c.
~~~

~~~
REPORT zhello_world.
INCLUDE zhello_world_top.

PARAMETERS: p_nome(20) TYPE c.

CONCATENATE 'Olá' p_nome INTO c_saudacao SEPARATED BY ' '.

WRITE c_saudacao.
~~~

Caso tenha algum problema na execução deste exemplo. Tente primeiro ativar ambos os arquivos antes de executar. O botão de ativação é este destacado na imagem abaixo.

![](/public/images/2015/03/z-activate.png)

E por enquanto é isto. Esta foi apenas uma pequena introdução, espero que tenham gostado.