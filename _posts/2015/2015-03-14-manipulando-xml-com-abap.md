---
title: Manipulando XML com ABAP
layout: post
lang: pt
tags: [abap, sax reader, xml, xml dom, xml parser, xpath]
---

O XML tornou-se o padrão mais aceito para troca de informações entre sistemas pela sua simplicidade e versatilidade. Apesar de eu achar que ele está perdendo o posto de líder para o mais novo queridinho do pedaço, o JSON, ainda veremos muitos e muitos documentos XML sendo usado para integração de sistemas. Em toda linguagem de programação encontramos pelo menos mais de uma forma de se trabalhar com estes documentos. As mais usadas acabam sendo funções de string (substring, indexOf, etc.), XML DOM e SAX Parser.

Usar funções de string é bastante primitivo e pouco prático, mas funciona bem quando se precisa buscar um conjunto limitado e pequeno de tags. No caso de um XML de Nota Fiscal Eletrônica, buscar o número da nota por substring é bem simples.

Já **SAX (Simple API for XML) Parser** é bastante usado quando se precisa ter desempenho na leitura do XML, pois trata-se de uma leitura sequencial das tags, podendo ser abortada a qualquer momento. Este padrão não armazenada nada em memória durante a leitura, ele apenas dispara eventos a cada nova tag lida. Cabe a você programador interceptar os eventos e adicionar a lógica que você precisa. O lado negativo é que trata-se de uma técnica de somente leitura e forward-only, ou seja, a leitura é de cima para baixo, sem voltar atrás. Fiz algumas pesquisas e ainda não achei uma implementação de SAX para ABAP, todas as referências de SAX na SCN são para Java. Olha ai a oportunidade. É um baita desafio!

E por fim existe o **XML DOM Parser**, o mais completo — e também complexo — de todos os três. Trata-se de uma leitura completa do XML e criação de uma estrutura em forma de árvore mantida em memória. Este processo segue um padrão definido pela W3C, o que significa que é as outras linguagens também trabalham de forma semelhante. Usar XML DOM em Java é parecido com ABAP, C#, PHP, etc., a diferença está sempre na sintaxe da linguagem e não da interface das classes.
  
Uma das desvantagem do XML DOM é que ele é mais demorado e utiliza mais memória que o SAX Reader. Por outro lado, esta técnica permite a completa manipulação de um documento XML, seja ela leitura, escrita, adição ou remoção de tags.

Achei este exemplo gráfico bastante intuitivo de como fica a estrutura após o parsing de um XML.
  
Fonte: <http://pugixml.googlecode.com/svn/tags/latest/docs/manual/dom.html>

~~~xml
<?xml version="1.0"?>
<mesh name="mesh_root">
    <!-- here is a mesh node -->
    some text
    <![CDATA[someothertext]]>
    some more text
    <node attr1="value1" attr2="value2" />
    <node attr1="value2">
        <innernode/>
    </node>
</mesh>
~~~


![](/public/images/2015/03/dom_tree.png)


<p>
  Vejam que a arvore é composta de um conjunto de nós (node) e que cada nó pode ou não ter seus atributos. Cada nó também possui uma referência para outro conjunto de nós, chamados de filhos ou irmãos. É através deste relacionamento que é feito a navegação e leitura de um documento XML. Estas são as possíveis referências de cada nó:
</p>

<ul>
  <li>
    parentNode
  </li>
  <li>
    childNodes
  </li>
  <li>
    firstChild
  </li>
  <li>
    lastChild
  </li>
  <li>
    nextSibling
  </li>
  <li>
    previousSibling
  </li>
</ul>


<h3>
  Exemplos em ABAP
</h3>

<p>
  Há pelo menos três formas de se trabalhar com XML DOM. 
</p>

<ul>
  <li>
    <b>Leitura Fixa:</b> Neste caso a leitura é feita com os comandos de captura dos nós de forma que o programador sabe exatamente qual o caminho que precisa percorrer para chegar ao nó de interesse. O problema desta técnica é que se aparecer uma nova tag no meio do arquivo o programa pode quebrar, justamente por ele ser programado para um layout bastante específico.
  </li>
  
  <li>
    <b>Leitura Recursiva:</b> Utilizando a técnica de programação recursiva podemos ficar em loop dentro das propriedades <strong>childNodes</strong> de cada nó e fazer uma leitura de todos os elementos. O resultado final é algo bem semelhante ao SAX Reader, com a diferença que neste caso podemos modificar os nós.
  </li>
  
  <li>
    <b>XPath:</b> Trata-se de um padrão bastante completo e fácil de entender para obter tags em documentos XML. Esta é a melhor de todas na minha opinião, justamente pela alta legibilidade do código quando comparada com os dois métodos acima. Este padrão também é definido pela W3C o que facilita bastante a portabilidade do conhecimento deste recurso para qualquer outra linguagem de programação. A vantagem deste modelo é a facilidade de entender o código e a possibilidade de mudar algumas partes do layout XML sem se preocupar com alteração do código. Um dos pontos negativos é que se usado incorretamente pode degradar ainda mais o desempenho do programa.
  </li>
  
</ul>

<p>
  Estou colocando abaixo um exemplo de programa em ABAP para cada um dos itens acima.<br />
  A explicação está em forma de comentários.
</p>

<p>
  <b>Leitura Fixa</b>
</p>

~~~
REPORT zxml_fixo.

DATA: lv_xml TYPE string,
      lo_xml TYPE REF TO cl_xml_document,
      lo_node TYPE REF TO if_ixml_node.

DATA: lv_name TYPE string,
      lv_nf_num TYPE string,
      lv_nf_serie TYPE string,
      lv_nf_val TYPE string.

lv_xml = '<NFe><serie>01</serie><nNF>573512</nNF><dEmi>2014-05-06</dEmi><hEmi>16:40</hEmi><item nro="1"><cod>50</cod><desc>Agua</desc></item><item nro="2"><cod>10</cod><desc>Refrigerante</desc></item><totais><valor>R$ 50,00</valor></totais></NFe>'.
WRITE: lv_xml, /.

CREATE OBJECT lo_xml.
lo_xml->parse_string( stream = lv_xml ). "Faz a leitura do XML e cria a árvore de nós
lo_node = lo_xml->get_first_node( ). "Obtem o primeiro nó de nossa arvore. Neste caso o nó corresponde à tag <NFe>

lv_name = lo_node->get_name( ). "Imprime NFe

lo_node = lo_node->get_first_child( ). "Tag serie
lv_nf_serie = lo_node->get_value( ).
lo_node = lo_node->get_next( ). "Tag nNF
lv_nf_num = lo_node->get_value( ).
lo_node = lo_node->get_next( ). "Tag dEmi
lo_node = lo_node->get_next( ). "Tag hEmi
lo_node = lo_node->get_next( ). "Tag Item 1
lo_node = lo_node->get_next( ). "Tag Item 2
lo_node = lo_node->get_next( ). "Tag totais
lv_nf_val = lo_node->get_value( ).

* Notem que se tivesse mais uma tag item o programa já não funcionaria

WRITE: 'NF', lv_nf_num, 'Serie', lv_nf_serie, 'Total', lv_nf_val.
~~~


<p>
  <b>Leitura Recursiva</b>
</p>


~~~
REPORT zxml_recursivo.

DATA: lv_xml TYPE string,
      lo_xml TYPE REF TO cl_xml_document,
      lo_node TYPE REF TO if_ixml_node.

lv_xml = '<NFe><serie>01</serie><nNF>573512</nNF><dEmi>2014-05-06</dEmi><hEmi>16:40</hEmi><item nro="1"><cod>50</cod><desc>Agua</desc></item><item nro="2"><cod>10</cod><desc>Refrigerante</desc></item><totais><valor>R$ 50,00</valor></totais></NFe>'.
write: lv_xml, /.

CREATE OBJECT lo_xml.
lo_xml->parse_string( stream = lv_xml ). "Faz a leitura do XML e cria a árvore de nós
lo_node = lo_xml->get_first_node( ). "Obtem o primeiro nó de nossa arvore. Neste caso o nó corresponde à tag <NFe>

PERFORM read_node USING lo_node. "Inicia a função recursiva

FORM read_node USING value(lo_node) TYPE REF TO if_ixml_node.
  DATA: lv_name TYPE string,
        lo_child TYPE REF TO if_ixml_node.

  WHILE lo_node IS NOT INITIAL. "Faz um loop enquanto existirem nós válidos

    IF lo_node->num_children( ) > 0.
      lo_child = lo_node->get_first_child( ).

      IF lo_child->get_type( ) = lo_child->co_node_text. "Se o filho for um texto
        PERFORM print_text_node USING lo_node. "Imprime o conteudo e encerra este nó
      ELSEIF lo_child->get_type( ) = lo_child->co_node_element. "Se o filho for um nó
        PERFORM print_parent_node USING lo_node. "Imprime detalhes do pai
        PERFORM read_node USING lo_child. "Reinicia a função recursiva usando o filho
      ENDIF.

    ENDIF.

    lo_node = lo_node->get_next( ). "Obtem o próximo no

  ENDWHILE.
ENDFORM.

FORM print_text_node USING value(lo_node) TYPE REF TO if_ixml_node.
  DATA: lv_name TYPE string,
        lv_value TYPE string.

  lv_name = lo_node->get_name( ).
  lv_value = lo_node->get_value( ).
  WRITE: / '-', lv_name, ':', lv_value.
ENDFORM.

FORM print_parent_node USING value(lo_node) TYPE REF TO if_ixml_node.

  DATA: lv_name TYPE string,
        lv_value TYPE string,
        lo_attributes TYPE REF TO if_ixml_named_node_map,
        lo_attribute  TYPE REF TO if_ixml_node,
        lv_count TYPE i,
        lv_index TYPE i.

  lv_name = lo_node->get_name( ).
  WRITE: / lv_name.
  lo_attributes = lo_node->get_attributes( ).
  lv_count = lo_attributes->get_length( ).

  DO lv_count TIMES.
    lv_index = sy-index - 1.
    lo_attribute = lo_attributes->get_item( lv_index ).
    lv_name = lo_attribute->get_name( ).
    lv_value = lo_attribute->get_value( ).
    WRITE: lv_name, '=', lv_value NO-GAP.
  ENDDO.
ENDFORM.
~~~


<p>
  <b>XPath</b>
</p>


~~~
REPORT zxml_xpath.

DATA: lo_xml      TYPE REF TO cl_xml_document,
      lo_root     TYPE REF TO if_ixml_node,
      lo_xslt     TYPE REF TO cl_xslt_processor,
      lv_xml      TYPE string.

lv_xml = '<NFe><serie>01</serie><nNF>573512</nNF><dEmi>2014-05-06</dEmi><hEmi>16:40</hEmi><item nro="1"><cod>50</cod><desc>Agua</desc></item><item nro="2"><cod>10</cod><desc>Refrigerante</desc></item><totais><valor>R$ 50,00</valor></totais></NFe>'.
WRITE: lv_xml, /.

CREATE OBJECT lo_xml.
lo_xml->parse_string( stream = lv_xml ).

CREATE OBJECT lo_xslt TYPE cl_xslt_processor. "cl_xslt_processor é a classe responsável por fazer as buscas de XPath
lo_root = lo_xml->get_first_node( ).
lo_xslt->set_source_node( lo_root ). "Nosso processador deve começar do primeiro nó de nosso XML
* NOTA: Apesar de estarmos usando o nó raiz, é perfeitamente possível começar de um nó mais baixo da arvore, diminuindo assim o escopo da busca.

* Abaixo estão alguns exemplos de uso
PERFORM print_xpath USING '//item/cod' lo_xslt. "Procura todas as tags <cod> abaixo de <item>
PERFORM print_xpath USING '//item/desc' lo_xslt. "Procura todas as tags <desc> abaixo de <item>
PERFORM print_xpath USING 'dEmi' lo_xslt. "Procura todas as tags <dEmi> imediatamento abaixo da raiz
PERFORM print_xpath USING 'desc' lo_xslt. "Procura todas as tags <dmi> imediatamento abaixo da raiz. Neste caso não vai achar nenhuma
PERFORM print_xpath USING '//item[@nro="2"]/desc' lo_xslt. "Procura todas as tags <desc>, abaixo de <item> que tenha o atributo nro = 2

* Visite http://www.w3schools.com/xpath/xpath_syntax.asp para conhecer mais sobre a sintaxe de XPath

FORM print_xpath USING lv_expression TYPE string
                       lo_xslt TYPE REF TO cl_xslt_processor.

  DATA: lo_nodes    TYPE REF TO if_ixml_node_collection,
        lo_node     TYPE REF TO if_ixml_node,
        lo_iterator TYPE REF TO if_ixml_node_iterator,
        lv_value    TYPE string,
        lv_nodes_length   TYPE i.

  WRITE: /, 'XPath:', lv_expression.

  lo_xslt->set_expression( expression = lv_expression ). "Atributui ao processador qual vai ser a expressão a ser resolvida
  lo_xslt->run( '' ). "Faz a busca pelos elementos. Após finalizado, o resultado pode ser obtido pelo método get_nodes.

* Os próximos comandos são usados para fazer um loop no
* resultado e mostrar em tela seus valores.
  lo_nodes = lo_xslt->get_nodes( ).
  lo_iterator = lo_nodes->create_iterator( ).
  lv_nodes_length = lo_nodes->get_length( ).

  DO lv_nodes_length TIMES.
    lo_node = lo_iterator->get_next( ).
    lv_value = lo_node->get_value( ).
    WRITE: / '-', lv_value.
  ENDDO.
ENDFORM.
~~~


<h3>
  Modificando um XML em ABAP
</h3>


<p>
  Agora que já aprendemos várias formas de ler podemos passar para a próxima etapa e fazer um programa de exemplo para modificar um documento XML. Para saber todas as operações possíveis podemos olhar os métodos da interface <strong>IF_XML_NODE</strong>. Veja abaixo como ficaria um programa que altera o conteúdo de uma tag e remove uma outra tag. Em seguida coloquei uma imagem do estado final do XML.
</p>


~~~
REPORT zxml_modify.

DATA: lo_xml      TYPE REF TO cl_xml_document,
      lo_root     TYPE REF TO if_ixml_node,
      lo_xslt     TYPE REF TO cl_xslt_processor,
      lv_xml      TYPE string.

DATA: lo_nodes    TYPE REF TO if_ixml_node_collection,
      lo_node     TYPE REF TO if_ixml_node.

lv_xml = '<NFe><serie>01</serie><nNF>573512</nNF><dEmi>2014-05-06</dEmi><hEmi>16:40</hEmi><item nro="1"><cod>50</cod><desc>Agua</desc></item><item nro="2"><cod>10</cod><desc>Refrigerante</desc></item><totais><valor>R$ 50,00</valor></totais></NFe>'.

CREATE OBJECT lo_xml.
lo_xml->parse_string( stream = lv_xml ).

CREATE OBJECT lo_xslt TYPE cl_xslt_processor.
lo_root = lo_xml->get_first_node( ).
lo_xslt->set_source_node( lo_root ).

lo_xslt->set_expression( expression = '//serie' ).
lo_xslt->run( '' ).

* Até aqui não há novidade, estamos apenas obtendo o nó <serie>

lo_nodes = lo_xslt->get_nodes( ).
IF lo_nodes->get_length( ) > 0.
  lo_node = lo_nodes->get_item( 0 ).
* Depois de capturado o nó que desejamos manipular, basta utilizar o método set_value para mudar seu conteúdo
  lo_node->set_value( '02' ).
ENDIF.


* Já neste exemplo estamos obtendo o nó <dEmi> para removê-lo em seguida
lo_xslt->set_expression( expression = '//dEmi' ).
lo_xslt->run( '' ).

lo_nodes = lo_xslt->get_nodes( ).
IF lo_nodes->get_length( ) > 0.
  lo_node = lo_nodes->get_item( 0 ).

* <dEmi> é filho do nó principal que em nosso exemplo está armazenado em lo_root
* Por isto utilizamos o remove_child do pai do nó que desejamos remover.
  lo_root->remove_child( lo_node ).
ENDIF.

lo_xml->display( ).

* Quer um desafio? utilize a combinação dos métodos CREATE_SIMPLE_ELEMENT da classe cl_xml_document
* e o método APPEND_CHILD da interface if_ixml_node para adicionar um novo nó.
~~~


![](/public/images/2015/03/display-xml.png)


<p>
  A imagem acima foi tirada de dentro do próprio SAP ao chamar o método <strong>DISPLAY</strong> da classe <strong>CL_XML_DOCUMENT</strong>. Bem útil este formato de visualização, não é mesmo?
</p>