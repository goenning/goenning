---
title: Introdução à Testes Unitários em ABAP
layout: post
lang: pt
tags: [abap, automação de testes]
---

Ouvi falar de testes automatizados pela primeira vez ainda durante meu estágio. Lembro bem do momento em que estava criando um projeto no Microsoft Visual Studio e notei um tipo de projeto chamado de &#8220;Unit Tests&#8221;. Mal sabia eu que este viria a ser um recurso que tanto gosto. Posso dizer que tive um dejavú quando estava criando uma classe na transação se24 e vi uma opção chamada &#8220;Test Class (ABAP Unit)&#8221;.

![](/public/images//2015/03/se-24-test-class-abap.png) 

Fico bastante animado em saber que a SAP considera este recurso tão importante que até criou uma implementação disto no próprio standard, diferente de outras plataformas onde dependemos de bibliotecas externas. Convido você leitor a participar comigo deste estudo e espero convencê-lo do quão útil — apesar de trabalhoso — ele é.

## O conceito de Testes Unitários

A parte teórica é fácil, pois o conceito é bem simples. Difícil mesmo é ter como prática criar bons testes. Explico em seguida.

Testar faz parte do fluxo de trabalho de nós desenvolvedores. O processo de teste mais comum é o manual onde executamos uma transação ou função com alguns parâmetros e avaliamos a saída. Se for um relatório verificamos se os dados estão corretos, caso contrário usamos outra transação para verificar o resultado. Caso o resultado esteja correto passamos para outro cenário de teste. Já se estiver errado, corrigimos o programa e reiniciamos o teste. Quando encontramos um problema em um cenário de teste posterior e precisamos ajustar o programa, é comum que tenhamos que reiniciar os testes desde o primeiro cenário. Todos fazem isto? Provavelmente não. Eu também muitas vezes não faço. Por quê? Por que testar é chato, leva tempo, é repetitivo e confiamos em nossa habilidade, sabemos que aquele código que acabamos de inserir não terá nenhum efeito colateral. Quem é que nunca pensou nisto, não é mesmo?

Uma vez ouvi dizer que programador bom é programador preguiçoso. Não aquela preguiça de não ter vontade de trabalhar ou de resolver os problemas, e sim uma preguiça boa, aquela que faz com que ele crie programas que trabalhem por eles. Testes automatizados tem tudo a ver com isto. São programas que testam programas, fazem o trabalho sujo de testar e testar diversas vezes sem reclamar. 

![](/public/images//2015/03/abap-unit-result-display.png)

Uma classe de testes unitários é possui uma lógica que executa um programa ou uma função passando parâmetros de entrada e avalia se a saída está de acordo. Para cada cenário de teste criamos um método em nossa classe que é conhecido como **Caso de Teste**. Para um programa que tenha 5 variações de função podemos criar um teste de unidade que executa cada uma de suas variações. Ao final da execução dos testes um relatório como este da imagem ao lado é apresentado. A imagem em questão mostra que o cenário onde o XML com espaços não está funcionando como deveria.

O tempo que o computador leva para executar estes 5 testes será algumas centenas de vezes mais rápido que um ser humano. Isto permite também que os testes sejam executados constantemente, várias vezes seguidas, conforme o programador vai alterando o código. Este tipo de fluxo de trabalho garante que ao alterar o código não estou estragando nada do que já estava funcionando, mas isto só é verdade se todos os cenários estiverem cobertos por testes. 

Para você ter uma ideia, o projeto [Sharp SAP RFC](/2015/03/14/novo-projeto-sharp-sap-rfc/) foi escrito em C# e teve o uso de testes unitários desde a primeira linha de código. O projeto atualmente possui 92 casos de testes que são executados em 11 segundos. São todos estes cenários que me dão segurança de colocar novos recursos na biblioteca sem correr o risco de publicar uma nova versão com novidades e estragando algo que antes já estava funcionando.

![](/public/images//2015/03/vs-ssr-testes.png)

SharpSapRfc: 92 casos de teste em 11 segundos

## E ai, gostou?

Quando estamos em um fluxo de trabalho onde escrevemos classes de testes para o sistema temos que ter em mente que estamos duplicando a quantidade de código escrito. No primeiro momento isto pode parecer que vamos levar o dobro do tempo para programar uma mesma funcionalidade. Mas cá entre nós, de todo o tempo que nós passamos em atividades de desenvolvimento de software, uma parcela pequena é gasta em codificação, pois as atividades de análise do problema, concepção de solução, leitura de código e teste manual compreende grande parte de nosso trabalho. Escrever teste automatizados certamente vai aumentar a quantidade de código que escrevemos, ou seja, no final teremos mais código para ler e dar manutenção. Este é um dos pontos a serem colocados na ponta do lápis na hora de optar por programar com testes. Por experiência própria posso dizer que é muito bom de se trabalhar desta forma, no começo demora um pouco mais para sairmos do lugar mas com o tempo vamos ganhando agilidade e os testes vão nos dando segurança de mudar, evoluir e até refatorar o nosso código, garantindo que os requisitos de negócio antigos continuam a ser atendidos mesmo após a implementação de novas regras.

Em muitos casos codificar uma classe de teste pode ser mais difícil do que a própria classe de negócio. Podemos perceber isto claramente quando nossa classe precisa se comunicar com recursos externos, como um banco de dados ou um arquivo texto. Cada caso de teste precisa de um cenário bastante específico para que o teste seja executado. Isto significa que qualquer intervenção externa de um usuário sobre o ambiente pode causar um estrago aos testes. Imagina que estamos escrevendo um teste que altera os dados do cliente com o código 10202 e alguma outra pessoa acessa o sistema SAP e remove este cliente do sistema. Nossa classe de teste vai começar a falhar sendo que não há nada de errado com o código, mas sim com os dados de que nosso cenário é dependente. Em outras plataformas como .Net e Java é comum o uso de dublês, que adicionam uma complexidade a mais nos testes mas garantem a completa independência de nossos testes sobre o banco de dados ou qualquer outro recurso externo.

Uma outra possível solução para este problema seria usar os recursos de **SETUP** e **TEARDOWN**. Trata-se de dois eventos que são executados no início e no término de cada teste respectivamente. Seria possível através destes eventos fazer uma preparação do ambiente e um rollback após cada teste, garantindo assim que o sistema fique em um estado consistente antes de iniciar a execução dos testes. Mas isto é assunto para outro artigo quando estivermos fazendo um exemplo prático.

A ideia aqui era apenas introduzir o assunto de testes unitários e passar esta boa notícia de que também podemos explorar este fantástico recurso em ABAP.

Até mais.