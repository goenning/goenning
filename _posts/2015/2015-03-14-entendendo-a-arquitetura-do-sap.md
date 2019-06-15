---
title: Entendendo a arquitetura do SAP
layout: post
lang: pt
tags: [abap, application server, arquitetura, camadas, sap]
---

![](/public/images/2015/03/arquitetura-sap-r31.gif)

## Arquitetura do SAP e suas 3 camadas

Antes de estudar qualquer assunto relacionado à linguagem ABAP é necessário conhecermos um pouco sobre a plataforma onde nossos programas são criados e executados.

O SAP usa a famosa arquitetura cliente-servidor de três camadas: **apresentação**, **aplicação** e **dados**.

### Camada de Apresentação:

 São os componentes visuais utilizados pelos usuários para interagir com o sistema SAP, seja ele o SAPGui, um navegador de internet ou um dispositivo móvel. Esta camada é considerada &#8220;thin&#8221; (magra) ou burra, pois aqui não há lógica de negócio, sua responsabilidade é apenas coletar dados digitados pelo usuário e enviar à camada de aplicação, aguardar o retorno e em seguida mostrar em tela o que foi retornado pela camada de aplicação.

### Camada de Aplicação:

 É o cérebro do sistema SAP, local onde os programas ABAP são iniciados e executados. Esta camada é responsável por receber solicitações da camada de apresentação e decidir o que deve ser feito. Toda a regra de negócio e fluxo de navegação em telas se encontra neste local. A comunicação entre as camadas de Aplicação e a de Apresentação é considerada de baixo volume e seu conteúdo é comprimido, justamente para reduzir tráfego e diminuir o tempo de resposta.

É muito comum grandes sistemas SAP terem dezenas de servidores para atuarem neste papel. Quando há mais de um **Application Server** para o mesmo sistema é necessário que se tenha um outro componente chamado de **Message Server**. Neste caso a camada de integração para de interagir diretamente com Application Server e passa a encaminhar as solicitações para o Message Server. Seu papel é de decidir para qual Application Server a solicitação deve ser encaminhada.

### Camada de Dados:

É aqui que fica o banco de dados que contém todos os dados do sistema. Trata-se de uma camada que tem pouco a ver com a SAP, pois aqui quem manda é o Sistema Gerenciador de Banco de Dados. O sistema SAP possui suporte a uma grande gama de SGBC, alguns dos mais usados são Oracle, DB2 e SQL Server.

Este modelo de camadas permite que tenhamos várias máquinas com papéis diferentes, por exemplo: 1 servidor de banco de dados, 4 servidores de aplicação e 100 estações de SAPGui. No caso de uma instalação do MiniSAP as 3 camadas estão em uma única máquina. Mas você também pode optar por usar o SAPGui fora do servidor do MiniSAP, neste caso seu servidor estaria fazendo o papel apenas da camada de aplicação e banco de dados.

## Applicaton Server

Dentre estas 3 camadas o Application Server destaca-se por ser onde a maioria dos recursos fundamentais estão presentes.

![](/public/images/2015/03/as-abap.gif)

Os principais componentes do **Application Server** são o **Dispatcher**, o **Work Process** e **Shared Memory**.

O work process é uma unidade básica de processamento que é responsável por executar uma etapa do programa em que o usuário está solicitando. Toda solicitação da camada de apresentação é direcionada à um work process que esteja disponível através do Dispatcher, entenda este componente como um load balancer interno ao application server. É importante frisar que não há como garantir que toda requisição oriunda de uma mesma sessão será sempre endereçada ao mesmo work process. Portanto é necessário que todos os dados de sessão estejam disponíveis para todos os processos. No SAP este local é chamado de Shared Memory, um repositório de dados — não só de sessão — mas também de cache e informações de lock, por exemplo.

O que o SAP garante é que toda sessão de usuário é sempre direcionada para o mesmo Application Server através do Messaging Service.

Podemos notar que a **quantidade de work process x quantidade de usuários/processos ativos** pode ser uma dos fatores de atraso no tempo de resposta no sistema SAP. Normalmente a quantidade de processos é muito menor que a quantidade de usuários, pois nem todos os usuários estarão executando programas simultaneamente, alguns podem estar parados na tela olhando um relatório sem consumir recursos. Caso não haja um work process disponível para atender uma solicitação da camada de aplicação, esta é enfileirada e fica no aguardo da liberação de um processo.

Esta é apenas uma introdução à este assunto que é tão abrangente. Para quem tem interesse em aprender mais sobre isto, procure por cursos ou conteúdo de **BASIS**. O time de BASIS é responsável pela elaboração, administração, manutenção e monitoramento de ambientes SAP. O importante para nós desenvolvedores é termos em mente que durante o desenvolvimento em ABAP grande parte do processamento ocorre no Application Server e que os work processes possuem um papel fundamental no entendimento da solução.

Um abraço e até mais.