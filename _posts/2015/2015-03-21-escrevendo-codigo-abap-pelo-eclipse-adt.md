---
title: Escrevendo código ABAP pelo Eclipse ADT
layout: post
lang: pt
tags: [ abap, ambiente, eclipse adt]
---
**Eclipse ADT** (ABAP Development Tool) é uma versão da IDE mais conhecida com complementos que permitem que possamos programar em ABAP fora do SAP. A [SAP já alertou](http://scn.sap.com/docs/DOC-29113#jive_content_id_Will_SAP_stop_investing_in_the_ABAP_workbench_SE80_or_even_disable_it) de que no futuro novos recursos exclusivos serão adicionados ao ADT. Então este será o futuro, queira você quer ou não. Quanto antes você se acostumar, melhor. Vejamos alguns outros motivos para optar pelo uso do Eclipse ADT.

* **Abas:** Se você também odeia programar com aquelas várias janelas do SAP aberta, seus problemas definitivamente acabaram. A base do Eclipse é trabalhar com abas para tudo e com o ABAP não é diferente. Podemos abrir inúmeras abas e navegarmos entre diversos objetos com facilidade. Diga adeus ao limite de telas do SAP, pois você pode abrir quantos objetos você quiser. É possível também abrir duas abas lado a lado, permitindo comparar e copiar código com maior facilidade.
    
![](/public/images//2015/03/eclipse-adt-abas.png)

* **Apenas código:** Uma das características que mais odeio no workbench é que alguns objetos são apresentados em forma de um formulário. Já viu que para adicionar um novo parâmetro em uma função você precisa acessar a aba de parâmetros e preencher um ALV? É quase um gerador de código. Pior ainda é criar uma classe. Quem aqui nunca se perdeu navegando na tela da se24 na procura por um método e seus parâmetros? É tudo muito confuso. Parece que o workbench tentou facilitar, mas programadores querem ver código e o Eclipse ADT faz exatamente isto. Tudo o que você vai fazer aqui é ler e escrever código e mais código. 
![](/public/images//2015/03/eclipse-adt-salv-table.png)
  
* **Recursos nativos:** O editor do Eclipse já vem com inúmeros recursos e muitos destes estão habilitados para uso em ABAP também. Renomear, bookmark, depuração, refatorar, lista de tarefas, janelas personalizadas a seu gosto, ambiente de execução de testes unitários, entre outros. 
![](/public/images//2015/03/eclipse-adt-rename.png)

Mas nem tudo são flores. Alguns recursos do workbench ainda não estão disponíveis. Não é possível por exemplo visualizar objetos do dicionario de dados, editar elementos de texto, entre outros. Nestes casos o Eclipse ADT é inteligente o suficiente e abre automaticamente o SAPGui já na tela que possui a informação solicitada. Sabe o que é o melhor? Nestes casos SAPGui é aberto dentro de uma aba do próprio Eclipse, é totalmente funcional e bem prático.

![](/public/images//2015/03/eclipse-adt-sapgui.png)

Lembrando que por mais que estejamos fora do SAP, todo objeto editado é salvo diretamente no banco do SAP, não existindo uma cópia local dos arquivos. Mas não me surpreenderia se um dia o Eclipse ADT começar a oferecer suporte à programação offline. Poderíamos fazer uma copia local de alguns programas e editar mesmo estando distante do SAP. Quando a conectividade for re-estabelecida os objetos seriam incronizados. Não seria ótimo?

Há também uma restrição quanto à algumas funções do ADT que só estão disponíveis para determinadas versões do ABAP. A SAP mantêm esta lista de funcionalidades e qual a versão necessária.

[ADT Feature Availability Matrix for AS ABAP Releases](http://scn.sap.com/community/abap/eclipse/blog/2013/06/05/adt-feature-availability-matrix-for-as-abap-releases)

## Instalação e Configuração

Para começar precisamos fazer o download do Eclipse. Vamos baixar a versão chamada de Luna através do endereço oficial <http://www.eclipse.org/downloads>. Para usar o Eclipse basta extraí-lo do arquivo compactador e clicar no executável, não sendo necessário fazer uma instalação. Verifique também se você tem instalado o Java 1.7 e certifique-se de que a arquitetura do Java (32 Bit ou 64 Bit) corresponde ao do Eclipse.

  1. Com o Eclipse aberto, acesse **Help -> Install New Software&#8230;**.
  2. Clique em **Add&#8230;** e preencha o campo **Location** com **https://tools.hana.ondemand.com/luna** e clique em **Ok**.
  3. Selecione **ABAP Development Tools for SAP NetWeaver** e clique em **Next >**.
  4. Continue avançando, confirme os termos de aceite e clique em **Finish**.
  5. Aguarde enquanto o Eclipse faz o donwload e instalação dos componentes escolhidos.

![](/public/images//2015/03/eclipse-new-abap.png)

Vamos começar criando um projeto do tipo **Projeto ABAP**. Logo no começo já precisamos informar em qual sistema queremos conectar. Ao clicar em **Procurar** são listadas todas as mesmas entradas que estão no SAPGui. Assim fica fácil, não é? Siga a tela de passo-a-passo e informe o mandante, usuário e senha. Cada Projeto ABAP corresponde à um sistema SAP, se você tiver que desenvolver em múltiplos sistemas, crie um projeto para cada um deles.

Agora já estamos prontos para começar a programar. O uso do Eclipse ADT é bastante auto explicativo e de fácil entendimento. Na esquerda temos a lista de pacotes e seus objetos, basta navegar até o objeto de interesse e clicar duas vezes. O código fonte do objeto é aberto na janela principal e a partir daqui é só editar o código a vontade. O atalho para ativar o objeto atualmente aberto é CTRL+F3, mas também podemos ativá-lo através menu de contexto clicando com o botão direito no código. Outra opção é o CTRL+SHIFT+F3 que abre uma lista de todos os objetos pendentes de ativação.


![](/public/images//2015/03/eclipse-unit-test.png)

Espero que tenham gostado deste artigo o tanto quanto gostei de escrevê-lo. A minha sugestão é que comecem a se acostumar com a ideia de usar Eclipse, pois esta será a ferramenta unificada da SAP para qualquer tipo de desenvolvimento. Já é possível usar o Eclipse para ABAP, SAPUI5, WebDynpro, SAP PI, SAP MII entre outras. Vai ficar de fora desta? Quero ver todo mundo usando Eclipse de agora em diante.

Um grande abraço!