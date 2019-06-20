---
title: Variáveis de Sistema do ABAP
layout: post
lang: pt
tags: [abap]
---

O ABAP possui uma extensa quantidade de variáveis de sistema para as mais diferentes ocasiões. São variáveis que estão disponíveis em todos os programas e sempre possuem o prefixo **sy-**, por exemplo **sy-uname** e **sy-subrc**. Neste post vou mostrar somente os mais usados, para uma lista completa de todas as variáveis disponíveis acesse [este endereço](http://help.sap.com/saphelp_wp/helpdata/en/7b/fb96c8882811d295a90000e8353423/content.htm) do help.sap.

### SY-UNAME

Retorna o nome do usuário logado no sistema.

### SY-LANGU

Retorna o idioma selecionado no logon.

### SY-DATUM

Retorna a data atual do sistema.

### SY-UZEIT

Retorna a hora atual do sistema.

Exemplo de uso de todas as variáveis acima.

~~~
REPORT z_guia_do_codigo.

WRITE: 'Meu nome é', sy-uname, 
       'e estou usando o sistema no idioma', sy-langu NO-GAP, '.', 
       'Hoje é dia', sy-datum, 'e', sy-uzeit, 'horas.'.

"O seguinte texto será é impresso em tela.       
"Meu nome é BCUSER e estou usando o sistema no idioma EN. Hoje é dia 16.03.2015 e 18:20:41 horas.
~~~

### SY-UCOMM

Retorna o nome do botão que o usuário clicou na barra de pressionado. Isto é bastante usado nos programas de Module Pool.

### SY-SUBRC

A variável SY-SUBRC armazena o código de retorno de grande parte dos comandos ABAP. É uma boa prática sempre que executar um comando ABAP, verificar se o retorno desta variável é igual a 0, se for é porque o comando foi executado com sucesso. Caso o retorno seja diferente de 0, é porque deu algum problema durante a execução. 

~~~
REPORT z_guia_do_codigo.
DATA: name(20) type c,
      last_name(20) type c,
      full_name(40) type c.

name = 'Guilherme'.
last_name = 'Oenning'.

CONCATENATE name last_name INTO full_name SEPARATED BY ' '.
IF sy-subrc = 0.
  WRITE: 'Deu certo, seu nome é', full_name.
ELSEIF sy-subrc = 4.
  WRITE: 'Não foi possível concatenar completamente os dois campos.'.
ENDIF.
~~~

Para cada comando existe uma lista de possíveis códigos de retorno. No caso do comando CONCATENA, sy-subrc será 0 em caso de sucesso e 4 caso a variável de destino não possa comportar a concatenação dos campos de entrada. Já para comandos SQL o valor 0 é usado quando o SELECT retornou pelo menos um registro. O valor 4 é retornado quando não foi retornado nenhum registro do banco de dados. 

A seguinte página contém uma lista de todos os comandos e seus possíveis retornos.

<http://wiki.scn.sap.com/wiki/display/ABAP/Sy-Subrc>

### SY-INDEX

A variável SY-INDEX armazena o contador de iteração de um laço de repetição DO&#8230;ENDDO e WHILE&#8230;ENDWHILE. Ele é semelhante à variável **i** que normalmente é declarada em laços de repetição **for** em outras linguagens. Veja abaixo um exemplo de uso.

~~~
DO 4 TIMES.
  IF sy-index = 2.
    CONTINUE.
  ENDIF.
  WRITE sy-index.
ENDDO.
~~~

O retorno deste código será a impressão dos números 1, 3 e 4, pois quando a variável SY-INDEX for igual a 2, a instrução CONTINUE fará o laço pular para a próxima iteração. 

Cuidado com o uso de SY-INDEX dentro de laços de repetição encadeados, pois como estamos falando de uma única variável para dois laços, esta mesma variável será atualizada quando entrar no laço de dentro. Veja um exemplo de como isto pode ser confuso. Temos que armazenar o primeiro SY-INDEX em uma variável se quisermos utilizá-lo dentro da próxima iteração. O curioso é que quando a instrução sai do laço interno a variável de ambiente SY-INDEX volta ao valor do índice do laço principal.

~~~
REPORT z_guia_do_codigo.
DATA: lv_index TYPE i.

DO 4 TIMES.
  lv_index = sy-index.
  DO 2 TIMES.
    IF lv_index = 4 AND sy-index = 2.
      WRITE: 'Esta será a ultima iteração!', /.
    ENDIF.

    WRITE: 'Index:', sy-index, /.
  ENDDO.
ENDDO.
~~~

### SY-TABIX

O SY-TABIX é semelhante ao SY-INDEX, com a diferença de que este é para uso em laços de repetição LOOP&#8230;ENDLOOP, usados para fazer iteração em uma tabela interna. Esta variável irá conter o número do índice da linha atual dentro da tabela interna que está sendo trabalhada.

~~~
LOOP AT alunos INTO wa_aluno.
  IF sy-tabix = 1.
    WRITE: 'Este é o primeiro registro da tabela.', /.
  ENDIF.

  IF sy-tabix = 4.
    WRITE: 'Este é o quarto registro da tabela.', /.
  ENDIF.

  IF sy-tabix = lines(alunos).
    WRITE: 'Este é o último registro da tabela.', /.
  ENDIF.
ENDLOOP.
~~~