---
title: ABAP e seu tipo booleano
layout: post
lang: pt
tags: [abap]
---
Se você já programa em outra linguagem e leu o artigo sobre [tipos de dados](/2015/03/11/tipos-de-dados-variaveis-e-constantes/) deve ter percebido que não falei nada sobre o **Boolean**. Não comentei sobre isto, pois ele **não existe** no ABAP. Isto é verdade, por mais estranho que pareça. Este é o tipo de dado mais primitivo que pode existir, afinal é o único que cabe em um único **bit**, e mesmo assim não existe.

Apesar da inexistência encontraram-se alternativas para remediar a falta deste recurso. Com o tempo tornou-se padrão tratar booleanos como um **CHAR de 1 caractere** e usar as literais **X** para Verdadeiro e **" "** para Falso.

É bem simples, veja um exemplo de como fica a utilização.

~~~
REPORT zabapbool.

DATA: gv_finalizado(1) TYPE c.

gv_finalizado = 'X'.

IF gv_finalizado = 'X'.
  WRITE 'Sou um booleano verdadeiro!'.
ELSEIF gv_finalizado = ' '.
  WRITE 'Sou um booleano falso.'.
ENDIF.
~~~

## Type Group: ABAP

Para facilitar o entendimento, evitar erros, eliminar uso de literais e padronizar ainda mais, a SAP disponibilizou um **Type Group** chamado **ABAP**. Um type group é onde definimos tipos e constantes que poderão ser reutilizadas em outros programas. Eles pertencem ao dicionário de dados e portanto são criados na transação **SE11**.

Podemos acessar a transação SE11 e exibir o type group ABAP e verificar o que foi definido dentro dele.

![](/public/images/2015/03/se11-type-group-abap.png)

~~~
types:
  abap_bool type c length 1.

constants:
  abap_true      type abap_bool value 'X',
  abap_false     type abap_bool value ' ',
  abap_undefined type abap_bool value '-',
  abap_on        type abap_bool value 'X',
  abap_off       type abap_bool value ' '.
~~~

Veja que um dos primeiros tipos de dados declarado é **abap_bool**. Veja também que existem algumas constantes relacionadas ao tipo booleano. Vamos reescrever nosso exemplo acima usando estes novos tipos:

~~~
REPORT zabapbool.

TYPE-POOLS abap.

DATA: gv_finalizado TYPE abap_bool.

gv_finalizado = abap_true.

IF gv_finalizado = abap_true.
  WRITE 'Sou um Booleano verdadeiro!'.
ELSEIF gv_finalizado = abap_false.
  WRITE 'Sou um booleano falso.'.
ENDIF.
~~~

A essência continua a mesma, apenas usamos um padrão para emular booleanos no ABAP. Utilizar as constantes apenas serve de guia para que os programadores não usem literais como &#8216;S&#8217; e &#8216;N&#8217;, por exemplo.

Segundo o livro **Official ABAP Programming Guidelines**:

> Rule 6.11: Use the abap_bool Data Type for Truth Values
> 
> To explicitly handle truth values, use the abap\_bool type as a workaround for a real Boolean data type. A data object that is declared in this way is not supposed to contain other values than the corresponding constants, abap\_true and abap\_false (as well as abap\_undefined).