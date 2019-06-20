---
layout: post
title: Introdução ao ABAP
lang: pt
tags: [abap]
---
O ABAP é uma linguagem de programação proprietária da SAP e utilizado na construção e evolução do ERP SAP. Foi concebido como uma  linguagem de programação estruturada e posteriormente com o advento da programação orientada a objetos foi atualizada para suportar também este novo paradigma.

O ambiente de programação é completamente incorporado no ERP SAP, isto significa que não é possível executar código ABAP fora deste ambiente.

Algumas das principais características são:

**1) Não há arquivos de código fonte**

Em linguagens como Java ou C#, os arquivos de código fonte ficam na máquina do próprio desenvolvedor. Uma IDE como o Eclipse ou Visual Studio é usada para fazer as modificações nos arquivos e compilar o projeto para em seguida publicar em um servidor de . Já no caso do ABAP, o SAP é a própria IDE de programação e também o ambiente de execução do código. Isto significa que os arquivos são criados e armazenados diretamente dentro do banco de dados SAP, nenhum arquivo é mantido na máquina local do desenvolvedor.

**2) Controle de versão de código imbutido**

Esqueça SVN e o Git, pois o SAP já vem com um sistema de controle de versão embutido que apesar de não ser tão completo como as ferramentas convencionais, possui as funções básicas e mais importantes como o histórico de alterações e comparação de versões.

Além disto exite também o Transport Management System que é usado para fazer o deploy de objetos do ambiente de desenvolvimento para o ambiente de teste e produção. Em outros ambientes de programação é comum que cada projeto tenha sua ferramenta de deploy (normalmente um script).

**3) ABAP é compilado e interpretado**

Os códigos ABAP quando executados pela primeira vez são compilados para uma linguagem intermediária conhecida como ABAP Load. Após este processo a Virtual Machine (localizada no SAP Kernel)  é responsável por interpretar o Load e converter em linguagem de máquina. Afinal, assim como o Java, ABAP roda em diversos sistemas operacionais.

A transação SGEN pode ser usada para pré-compilar os programas, evitando que o usuário tenha que esperar a compilação ao usar o programa pela primeira vez.

**4) Sintaxe e Comandos**

A sintaxe do ABAP segue o mesmo conceito de outras linguagens de quarta geração e é frequentemente comparada ao COBOL. Os comandos são mais próximos da linguagem humana e muitas vezes são comandos específicos do ambiente.

Não vou explorar muito isto agora, pois este assunto será vastamente apresentado em todos os posts onde teremos código ABAP.

**5) Acesso ao banco de dados**

Por se tratar de uma linguagem 4GL, não temos as formalidades de uma linguagem convencional na hora de usar um banco de dados. Nada de abrir conexão, fazer a consulta, ler cada uma das linhas e depois fechar todos os objetos de ligação com o banco. No ABAP, basta escrever um comando SQL para que ele já interpretee execute-a no banco de dados em que já estamos conectado. Lembre-se de que programamos diretamente dentro do SAP e por isto já estamos sempre conectados ao banco de dados.

**6) Prefixo**

Dentro do ambiente SAP é necessário começar qualquer programa ou tabela usando a letra Z ou Y. Quando estiver analisando um objeto que comece com uma destas letras, saiba que este um objeto que não veio por padrão no SAP, provavelmente foi desenvolvido por alguém e implantado no sistema. Isto ajuda bastante na hora de identificar o que é standard do que é customizado no sistema. Não existe uma regra para distinguir o uso de Z ou Y. Apesar de Z ser a mais comum, o Y também está disponível. Inclusive não há nenhuma diferença técnica entre um e outro. Cabe a cliente definir uma regra para uso destas letras. Exemplos de uso:

  * Y é usado pela consultoria e Z pelo cliente. &#8211; Não gostei, pois em algum momento alguém vai ter que dar manutenção no código do outro. E quando isto acontecer o padrão vai por água abaixo.
  * Y é usado para objetos temporários (não devem sair do DEV) e Z são objetos oficiais. &#8211; Eu gostei deste. Podemos criar programas Y que servem unicamente para auxiliar na programação e por isto não devem estar no ambiente de QA e PRD.
  * Usar apenas o Z e esquecer o Y.

Independente de qual seja a regra, o importante é definir uma e seguir a risca. O Z normalmente é usado para formular piadas no mundo SAP. Alguém pode chamar o sistema de ZAP em referência à um sistema mais customizado do que padrão. Outro comum é o Z-Zão, um programa Z gigante e que faz tudo.

**6****) Preparada para multi-idioma**

Umas das boas práticas do ABAP é sempre usar text symbols ao invés de hard-coded strings. Ao usar text symbols é possível traduzir todos as mensagens e textos de um idioma para outro sem ter que alterar os programas. Este mesmo conceito é usado para colunas de grid, campos de pesquisa entre outros locais do sistema.

Mesmo que o escopo de seu projeto seja apenas um idioma, use sempre os text symbols, não são complicados de usar e facilitam bastante quando for necessário traduzir o sistema.

**8) Report e Dialog**

O SAP suporta basicamente dois tipos de programas, o Report e o Dialog.

O Report é um programa normalmente usado apenas para relatório. A interface segue um padrão, onde a primeira tela é a de seleção de filtros e a segunda a listagem de dados.

Já o Dialog é um conjunto de telas executadas em uma sequência definida por um programa. São as telas onde os usuários podem consultar, inserir, modificar e excluir dados.

Estes dois tipos de programas serão muito explorados nas próximos postagens.

**9) Transações**

É provável que transação seja o termo mais conhecido dentro do SAP. Nos os olhos dos usuários a transação é um programa executável, uma tela onde possa cadastrar alguns dados ou tirar um relatório. Já para nós programadores a transação deve ser entendida como um atalho para um programa. Após desenvolvido um novo programa, é necessário usar a transação SE93 para criar um novo código de transação e associar ao programa. Conhecer as transações é importante, pois a transição de um programa para outro dentro do ABAP é feito por estes códigos usando o comando CALL TRANSACTION.

**10) Enhancements**

Para quem não sabe, não é permitido modificar os programas standards SAP, isto significa que se quiserem mudar algum detalhe de uma tela de processamento ou relatório será necessário criar um novo programa com base no programa standard. Isto é péssimo, pois se um dia houver alguma atualização desta tela standard será necessário atualizar também a tela Z customizada. Para evitar este tipo de problema e incentivar o uso dos programas standards, o SAP disponibiliza um recurso conhecido como Enhancement, em português isto significa aperfeiçoamento ou melhoria.

Enhancement Points são pontos estratégicos em programas SAP onde é deixado um espaço para que o cliente consiga colocar um código customizado. Pode ser uma validação, um gatilho para uma integração entre outras coisas. Este é um assunto bem interessante e muito usado, então ainda devo estudar e escrever bastante sobre isto.

&nbsp;

Por enquanto é isto. Demorei bastante para escrever, pois tive que pesquisar e escrever ao mesmo tempo. Mas o estudo foi bem produtivo, consegui aprender muita coisa interessante. Espero que tenha sido útil para você leitor.

Abraço!