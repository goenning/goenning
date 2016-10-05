---
title: Adicionando segurança no uso do ICF e RFC
layout: post
comments: true
lang: pt
tags: [icf, security, sap, abap, rfc]
ref: adding-security-to-icf-and-soap-rfc
---

![](/public/images/2015/03/14/no_auth_rfcping.png)

Já falamos bastante de como funciona a RFC no SAP, então se você chegou agora e quer saber um pouco mais sobre RFC, sugiro a leitura deste conteúdo:

  * [Chamando funções .NET com SAP RFC](/2015/03/chamando-funcoes-net-com-sap-rfc/ "Chamando funções .NET com SAP RFC")
  * [Conectando o mundo externo ao SAP com o uso de RFC](/2015/03/conectando-o-mundo-externo-ao-sap-com-o-uso-de-rfc/ "Conectando o mundo externo ao SAP com o uso de RFC")
  * <http://www.guru99.com/rfc-remote-function-call.html>
  * [Conectando-se ao SAP com o uso de SOAP RFC](/2015/03/conectando-se-ao-sap-com-o-uso-de-soap-rfc/ "Conectando-se ao SAP com o uso de SOAP RFC")

Acontece que a RFC pode representar um risco para a segurança do ambiente SAP. Afinal ninguém gostaria que um vírus/invasor/alguém-mal-intencionado ficasse criando Pedidos de Compra e Ordens de Produção sem você saber, correto? Ou pior ainda, alguém poderia criar usuários via RFC.

As funções que criam estes documentos/usuário estão habilitadas para chamada remota (isto é um dos requisitos para uma função ser considerada uma BAPI), então **qualquer usuário** autenticado pode chamar estas funções.

![](/public/images/2015/03/bapi-user-create.png)
  
Felizmente o SAP possui uma excelente mecanismo de autorização e as RFC não ficaram de fora.

Existem 4 objetos de autorização que vamos abordar aqui, são eles: **S_RFC**, **S_RFCACL**, **S\_RFC\_ADM** e **S_ICF**.
  
Se você não sabe o que é ou como funciona um objeto de autorização, leia isto aqui antes:

  * <http://abap101.com/2009/01/11/como-funciona-o-authority-check/>
  * <http://www.abapzombie.com/guias/2011/02/24/abapzombie-guide-to-abap-parte-14-authority-check/>

Primeiro precisamos entender o funcionamento do **auth/rfc\_authority\_check**. Este parâmetro define qual o nível de verificação que será realizado em uma chamada RFC.
  
Os possíveis valores são:

  * ****: Não faz verificação nenhuma
  * **1**: Verificação ativa, mas somente uma vez para o mesmo contexto de usuário ou grupo de função
  * **2**: Verificação ativa, mas somente uma vez para o mesmo grupo de função
  * **3**: Não faz verificação de autorização, mas é necessário fazer login, exceto nas chamadas às funções RFC\_PING e RFC\_SYSTEM_INFO
  * **4**: Verificação ativa para todas as funções exceto RFC\_PING e RFC\_SYSTEM_INFO
  * **5**: Não faz verificação de autorização, mas é necessário fazer login, exceto nas chamadas à função RFC_PING
  * **6**: Verificação ativa para todas as funções exceto RFC_PING
  * **8**: Não faz verificação de autorização, mas é necessário fazer login em todas as chamadas
  * **9**: Verificação ativa para toda chamada de função, independente do contexto ou grupo de função

São todos bem parecidas, mudando apenas um detalhe ou outro. O valor padrão é 1, apesar de muitos usarem e recomendarem a opção 9.
  
Para quem gosto da opção técnica de entender como funciona, da uma olhada no código da função **AUTHORITY\_CHECK\_RFC**, é lá que este parâmetros é usado.

## S_RFC

Este é o objeto de autorização mais usado e mais importante na configuração dos perfis de usuários RFC. A configuração é bem simples, pois existem apenas 3 parâmetros de autorização. Pode ser feito a nível de módulo de função ou grupo de função. Quando atribuímos um único módulo de função, somente aquela função pode ser executada. Quando atribuímos um grupo de função, todas as funções do grupo podem ser executadas. Múltiplos objetos podem ser adicionados garantindo que várias funções estejam autorizadas.

**ACTVT:** Só existe a opção 16 &#8211; Executar.
  
**RFC_TYPE:** Os valores possíveis são FUGR ou FUNC, que representa grupo de função e módulo de função respectivamente.
  
**RFC_NAME:** É aqui que especificamos o nome do módulo de função ou grupo de função que queremos liberar acesso (pode ser mais de um). O que define que tipo de objeto você deve inserir aqui é o RFC_TYPE.

**Dica:** Para fazer chamadas RFC usando o SAP Connector é necessário liberar acesso à algumas funções básicas além das funções de negócio que você precisa. Basicamente são funções de verificação de conexão e consulta dos metadados de funções e estruturas. Podemos seguramente adicionar os grupos de função SYST, RFC1 e RFC_METADATA.

Veja um exemplo completamente preenchido. Z_SSRT é o grupo de função que possui as funções de negócio que meu usuário precisa acessar remotamente.

![](/public/images/2015/03/bapi-user-create.png)

Lembrando que este objeto define apenas se o usuário pode ou não executar uma determinada função. A própria função pode ter (e na maioria das vezes tem) seus próprios objetos de autorização relacionados ao negócio, como a criação de ordem, alteração de pedido, dentre outros.

## S_RFCACL

Existe um conceito no SAP RFC que é conhecido como Trusted e Trusting System. É basicamente um acordo de confiança entre dois sistemas configurado na SM59. Sistemas confiáveis não precisam trafegar usuário e senha nas conexões, mas para que isto funcione o usuário especificado na SM59 precisa possui o objeto de autorização S_RFCACL. É também neste objeto que é especificado se o usuário configurado pode se passar por outro usuário, por exemplo. Independente se for uma conexão trusted ou não, as verificações de autorização da **S_RFC sempre irão acontecer**.

## S\_RFC\_ADM

Usuários que tenham este perfil podem executar ações de criação, alteração e remoção de configurações da SM59. Usando o campo de autorização RCFTYPE é possível restringir qual tipo de conexão pode ser manipulada. Sinceramente não vejo motivo para que alguma aplicação tenha que fazer estes ajustes remotamente, pois normalmente isto é configurado pela equipe de BASIS.

## S_ICF

Quando fazemos a chamada de função remota usando o SOAP Handler (ICF) não existe verificação de autorização. Qualquer usuário pode chamar qualquer função remota via SOAP sem ter uma permissão adicional. O objeto de autorização **S_RFC** não é usado neste cenário. Isto não é legal, mas felizmente existe o objeto de autorização **S_ICF** para ajudar um pouco. Acontece que ele é muito simples, tão simples que chega a ser ruim.

Com o S_ICF podemos determinar **quais serviços** do ICF um determinado usúario pode acessar, mas ainda não conseguimos restringir **quais funções** do sap/bc/soap/rfc são permitidas.

Vejamos um exemplo de como configurá-lo.

Entre na transação SICF e vá até o serviço desejado, que em nosso caso é sap/bc/soap/rfc. Vá no campo **Autorização SAP** e informe um valor qualquer. Neste exemplo vamos colocar RFC\_CHCK. Na criação do perfil, quando incluímos o objeto S\_ICF, será solicitado dois parâmetros de autorização: **ICF_FIELD** e **ICF_VALUE**. Em ICF\_FIELD vamos preencher SERVICE e em ICF\_VALUE o mesmo valor digitado na configuração do serviço do SICF, que no nosso caso é RFC\_CHCK. Isto significa que o usuário que tiver este perfil pode executar qualquer serviço ICF que tenha o campo Autorização SAP igual a RFC\_CHCK ou vazio.

![](/public/images/2015/03/RFC_CHCK.png)

Já é alguma coisa, mas ainda assim não é seguro deixar um usuário com autorização para executar qualquer função remota. Achei estranho que não houvesse uma forma de restringir isto e então iniciei uma depuração na classe **CL\_HTTP\_EXT\_SOAPHANDLER\_RFC** a fim de encontrar algo que pudesse ajudar.

E não é que encontrei? Existe um trecho de código que faz uma verificação pelo nome da função sendo executada. A tabela **SRT\_WHITE\_LIST** determina quais são as funções que podem ser chamadas via SOAP. Se a tabela estiver vazia, todas as funções podem ser executadas. Se estiver preenchida com algum valor, somente as funções cadastradas na tabela podem ser executadas.

![](/public/images/2015/03/SRT_WHITE_LIST.png)

Evoluímos um pouco, mas ainda não é o ideal, pois esta verificação não depende de usuário, ou seja, uma vez que a função esteja liberada, qualquer usuário pode executá-la. Mas a combinação desta tabela com o objeto de autorização S_ICF já restringe bastante e está bem próxima do cenário perfeito. Melhor isto do que nada, certo?

Por hoje é só, um abraço e até mais!