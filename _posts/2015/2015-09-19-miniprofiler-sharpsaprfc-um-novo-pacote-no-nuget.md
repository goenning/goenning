---
title: 'MiniProfiler.SharpSapRfc: um novo pacote no NuGet'
layout: post
lang: pt
tags: [sap, miniprofiler, performance, .net]
ref: miniprofiler-sharpsaprfc-new-nuget-package
---

Já fazia algum tempo que havia lido sobre o [MiniProfiler](http://miniprofiler.com), mas nunca tive a oportunidade de testar. Recentemente comecei a usá-lo e tenho gostado tanto que resolvi compartilhar com vocês o resultado do estudo.

Trata-se de uma biblioteca pequena, fácil de instalar e de configurar, e que traz informações muito úteis relacionadas ao tempo de resposta e processamento de cada página web. Foi criado pelo time do Stack Overflow (o Google dos programadores) e é [utilizado por eles em produção](http://blog.codinghorror.com/performance-is-a-feature/). Um grande case de sucesso, não é mesmo?

Com o MiniProfiler é possível saber quanto tempo cada requisição HTTP está levando e principalmente quais as etapas do código que estão demorando mais para serem processadas. Deixe-o ligado em seu ambiente de desenvolvimento e a cada página navegada você verá no canto superior esquerdo uma pequena caixa informando quanto tempo levou para que a página fosse completamente carregada.

![](/public/images//2015/09/PsjLY.png)

É necessário adicionar um chamada ao MiniProfiler para cada trecho de código que você quer contabilizar o tempo de execução. Existem alguns pacotes no NuGet que disponibilizam formas automáticas de registrar o profiling sem ter que explicitamente codificar todas as classes.

Um exemplo de pacote é o [MiniProfiler.MVC4](https://www.nuget.org/packages/MiniProfiler.Mvc4/) que permite contabilizar o tempo de execução de **todos** os Controllers e da renderização de **todas **as views adicionando apenas meia dúzia de código. Sensacional, não é?

Normalmente aplicações web fazem grande uso de banco de dados SQL, e é aqui que normalmente encontramos o maior causador de lentidão. Felizmente existem pacotes que automatizam a coleta de indicadores de desempenho para estes recursos. Alguns exemplos de pacotes são [SQL](https://www.nuget.org/packages/MiniProfiler) (qualquer banco de dados), [Entity Framework](https://www.nuget.org/packages/MiniProfiler.EF6/), [MongoDb](https://www.nuget.org/packages/MiniProfiler.MongoDb), [Raven](https://www.nuget.org/packages/MiniProfiler.Raven/) e &#8230;

## MiniProfiler.SharpSapRfc

Chamadas de funções no SAP podem ser bem lentas, principalmente quando estamos lidando com funções standard. Utilizar um profiling nestes casos ajuda bastante na hora de encontrarmos o vilão em uma requisição demorada.

É para estes casos que criei a biblioteca **MiniProfiler.SharpSapRfc.** Com esta biblioteca é possível fazer o profiling de **todas** chamadas remotas ao SAP quando utilizado com [SharpSapRfc](https://www.nuget.org/packages/SharpSapRfc/). Seu código está no [GitHub](https://github.com/goenning/MiniProfiler.SharpSapRfc) e os binários disponíveis no [NuGet](https://www.nuget.org/packages/MiniProfiler.SharpSapRfc/).

As instruções de instalação e uso estão na página inicial do projeto no GitHub.

Abaixo estão duas imagens de uma demonstração que montei. Neste caso estou usando [SharpSapRfc.Soap](https://www.nuget.org/packages/SharpSapRfc.Soap/), mas o profiler funciona perfeitamente com SharpSapRfc.Plain, tanto [x64](https://www.nuget.org/packages/SharpSapRfc.Plain.x64/) quanto [x86](https://www.nuget.org/packages/SharpSapRfc.Plain.x86/).

![](/public/images//2015/09/profile-sap-1.png)
![](/public/images//2015/09/profile-sap-2.png)

Espero que seja tão útil para você quanto está sendo para mim.

Abraço,

Guilherme