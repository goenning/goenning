---
title: Como funciona o Binary Search
layout: post
comments: true
lang: pt
tags: [abap, binary search, performance, tabelas]
---
Todo programador ABAP já ouviu falar que devemos utilizar a instrução **BINARY SEARCH** sempre que fizermos leituras em tabela internas por questões de performance. Podemos perceber a grande diferença no tempo de processamento através [desta análise comparativa entre diferentes leituras nos três tipos de tabela](/2015/03/18/performance-de-leitura-por-tipo-de-tabela/). A leitura sequencial standard começa a apresentar sérios problemas a medida que a quantidade de registros vai aumentando. As outras leituras, inclusive a binária, mantêm-se estável durante muito mais tempo, mostrando assim serem mais eficientes.

Mas você sabe como realmente funciona a pesquisa binária? Não? Então vem comigo.

## Binary Search

Binary Search é um algoritmo de busca muito usada também em outras linguagens, não sendo algo exclusivo do ABAP. Talvez você até tenha aprendido isto na faculdade, pois os professores adoram ensinar isto nas aulas de estruturas de dados. Você deve até implementado sua própria versão do algoritmo em C, mas não deve se lembrar. Vamos então refrescar a sua memória.

Trata-se de um algoritmo que — diferente da Linear Search — não precisa passar por todos os elementos da tabela para encontrar o registro de interesse, o que torna sua execução muito mais rápida. O pré-requisito para utilizar este algoritmo é que a tabela a ser pesquisa precisa estar ordenada de forma crescente pelo(s) campo(s) que usaremos na busca.

Para nos ajudar na explicação e entendimento da lógica vamos utilizar a seguinte lista de números [1,9,13,40,56,70,78,81,90,98] e procurar pelo número 78.
  
O algoritmo começa obtendo o valor exatamente no meio da lista, que no nosso caso é 56. Como 56 é menor que 78, então o que procuramos está depois do meio da tabela. Neste momento passamos a ignorar tudo o que está antes do 56 e só precisamos procurar a partir deste número para o final. Nossa lista agora é composta apenas dos números [70,78,81,90,98] e o número do meio é 81, que neste caso é maior que 78. A lista é reduzida mais uma vez e agora contém apenas os números [70,78,81]. Na próxima busca o valor do meio será 78, que é exatamente o número que estávamos procurando. 

Foram necessárias 3 iterações para encontramos o valor pretendido, muito menos do que as 7 iterações se tivéssemos usado Linear Search. Imagine agora a diferença caso nossa tabela tivesse milhares de registros.

Reforço mais uma vez que a tabela **precisa estar ordenada**, caso contrário o algoritmo vai se perder e não vai conseguir encontrar o registro desejado. Se você estiver começando agora a programar em ABAP, anote ai, pois um dia você vai esquecer disto. Quando este dia chegar o programa não vai funcionar e você irá arrancar os cabelos durante o debug. Vai dizer que é bug do ABAP, mas no fundo é apenas uma tabela não ordenada.

Ah, tem outro detalhe. Vale a pena usar Binary Search para tabelas com poucos registros? A resposta é **não faz diferença**. Ok, mas devo usar? Eu defendo a ideia de que, sempre que possível, deve-se optar por fazer leituras usando Binary Search, independente do tamanho da tabela. Nestes casos é mais uma questão de padronização do que de desempenho.