---
layout: post
title: Como reduzimos o tamanho inicial do JS e CSS em 67%
lang: pt
tags: [react, performance, web]
description: Recentemente fizemos várias melhorias no Fider para reduzir a quantidade total de bytes que são enviados aos navegadores. Por ser uma aplicação em React, nós focamos bastante em reduzir o JavaScript e CSS sem remover nenhuma funcionalidade. Neste post vamos compartilhar o que aprendemos, alguns conceitos e sugestões de como você pode fazer o mesmo em suas aplicações web.
ref: how-we-reduced-initial-jscss-size
---

![](/public/images/2018/11/bundle-size-improvements.png)

Fider foi construído com React e Webpack no frontend, por isto o tópicos abaixo são mais úteis para quem utiliza as mesmas tecnologias, mas muitos conceitos podem ser aplicados em Angular e Vue.js, por exemplo.

### Índice

- <a href="#webpack-bundle-analyzer">Webpack Bundle Analyzer</a>
- <a href="#cache-de-longa-duração-com-hash-de-conteúdo">Cache de longa duração com hash de conteúdo</a>
- <a href="#o-bundle-comum">O bundle comum</a>
- <a href="#divisão-de-código-baseada-em-rotas">Divisão de código baseada em rotas</a>
- <a href="#carregando-dependências-externas-por-demanda">Carregando dependências externas por demanda</a>
- <a href="#font-awesome-e-tree-shaking">Font Awesome e Tree Shaking</a>
- <a href="#trocando-grandes-pacotes-npm-por-opções-menores">Trocando grandes pacotes NPM por opções menores</a>
- <a href="#otimizando-o-bundle-principal-é-crucial">Otimizando o bundle principal é crucial</a>
- <a href="#tslib-apenas-para-usuários-de-typescript">TSLib (apenas para usuários de TypeScript)</a>

## Webpack Bundle Analyzer

[webpack-bundle-analyzer](https://www.npmjs.com/package/webpack-bundle-analyzer) é um plugin para o webpack que gera um mapa interativo de todos os bundles da aplicação. Isto foi crucial para nós entendermos quais são os módulos que estão dentro de cada bundle. Este mapa inclui não só o nome, mas também o tamanho de cada módulo.

*Se você não sabe qual a raiz do problema, como você o resolve?*

Este é um exemplo do que do mapa que o plugin gerou para nós.

![](/public/images/2018/11/bundle-size-initial-view.png)

Você notou aquele enorme **entities.json** dentro do vendor bundle? Aquele é um bom candidato a ser analizado para saber o que pode ser feito para minimizar o tamanho dos bundles.

## Cache de longa duração com hash de conteúdo

Cache de longa duração é o processo de instruir o navegador a fazer o cache de um arquivo por um longo tempo, por exemplo, 3 meses ou até mesmo 1 ano. Isto é muito importante para garantir que visitantes recorrentes não precisam fazer o download dos mesmos arquivos JavaScript/CSS novamente.

O navegador fará o cache dos arquivos baseados no seu nome completo, então se você precisa forçar o download de uma nova versão de seu bundle, é só garantir que o nome seja diferente. O webpack possui uma funcionalidade que gera o nome dos arquivos com base em algum critério.

Anteriormente estávamos usando a configuração **chunkhash** do webpack para fazer o cache de longa duração, mas em 99% dos casos, a melhor opção é usar **contenthash**. Esta configuração faz com que o webpack gere o nome do arquivo vaseado no seu conteúdo.

Esta configuração não reduz o tamanho total do bundle, mas certamente ajuda na hora de reduzir a quantidade de vezes que o usuários precisa fazer o download. Se o bundle não for alterado, não há necessidade de forçar nenhum download.

Caso queira saber mais sobre isto, visite https://webpack.js.org/guides/caching/

## O bundle comum

Combinar todos os pacotes NPM em um bundle separatado é algo bem comum em vários projetos. Isto é muito útil quando combinado com o cache de longa duração.

Pacotes NPM são alterados com menos frequência do que nosso código, então assim não forçamos os visitantes a fazer o download do bundle enquanto os pacotes do NPM não forem atualizados. Este bundle é normalmente chamado de **vendor bundle**.

Mas é possível levar este conceito um passo à diante.

Já pensou que no seu próprio código existem algumas partes que raramente são alteradas? Talvez você tenha alguns componentes básicos como Botão, Tabela, Alertas, etc. que foram criados à muito tempo atrás e que já faz tempo que não são alterados.

Estes módulos são excelentes candidados para fazerem parte do **common bundle**. Veja esta [PR #636](https://github.com/getfider/fider/pull/636) onde nós basicamente movemos todos os nossos próprios módulos de dentro de um diretório específico para um bundle comum.

Isto garante que, a não ser que nossos componentes básicos forem alterados, os visitantes do Fider não terão que fazer download deles novamente.

## Divisão de código baseada em rotas

Divisão de código (code splitting) é um tópico que está na moda. Esta técnica já existe há algum tempo, mas com a evolução das ferramentas e frameworks, fazer divisão de código ficou mais simples agora.

É muito comum ver aplicações web que colocam todo o conteúdo JS/CSS em um único arquivo. Este arquivo possui o código necessário para renderizar qualquer página da aplicação, mesmo que os visitantes esteja olhando apenas a página inicial. Não sabemos se os visitantes vão visiar a página de configuração, mas mesmo assim fazemos com eles empurramos todo o JavaScript para eles. O Fider sempre foi assim, porém alteramos isto recentemente.

A idea da divisão de código é gerar vários pequenos bundles, normalmente um por rota/página e outro bundle principal. O único bundle que é enviado para todos os usuários é o principal, que tem como objetivo carregar os outros bundles menos baseados para poderar renderizar as páginas necessárias.

Parece complicado, mas graças ao React e Webpack, o processo ficou mais simples. Para quem usa React <= 16.5, recomendamos o [react-loadable](https://github.com/jamiebuilds/react-loadable). Mas se você já está no React 16.6 ou mais recente, então é possível usar `React.lazy()` que foi recentemente adicionado.

- Nesta PR você consegue ver como <a href="https://github.com/cfilby">@cfilby</a> (obrigado!) adicionou divisão de código no Fider com react-loadable: [PR #596](https://github.com/getfider/fider/pull/596)
- Depois de migrar para React 16.6, trocamos a dependência externa por React.lazy [PR #646](https://github.com/getfider/fider/pull/646)

Também tivemos alguns raros problemas onde os visitantes estavam tendo dificuldades de baixar os bundles de forma assíncrona. A solução que implementamos foi documentada aqui [Como repetir quando React Lazy falha](https://goenning.net/2018/11/16/como-repetir-quando-react-lazy-falha/).

## Carregando dependências externas por demanda

Usando Webpack Bundle Analyzer, descobrimos que nosso vendor bundle continha todo o conteúdo do react-toastify, a biblioteca de mensagens/popups que usamos. Isto geralmente é esperado, mas no Fider, 95% nunca irão ver uma mensagem desta, afinal, existe pouquíssimos lugares em que mostramos estas mensagens. **Por que enviamos 30kB de JavaScript para todos os visitantes se eles não vão usar?**

Este problema é similar ao de cima, exceto que não estamos mais falando de rotas/páginas, e sim de uma funcionalidade que é usada em diversas páginas. É possível fazer  divisão de código no nível de funcionalidade?

Sim, é possível! Também não é tão complicado 😀

Basta trocar a importação do módulo de estático para dinâmico. Ficaria assim:

```javascript
// antes
import { toast } from "./toastify";
toast("Olá Mundo");

// depois
import("./toastify").then(module => {
  module.toast("Olá Mundo");
});
```

O Webpack fará o bundle do `toastify` e todas as suas dependências do NPM de forma separada. **O navegador só baixará este bundle quando o código for executado**. E case você tenha configurado o cache de longa duração, na segunda vez que este código for executado não será necessário baixar o bundle novamente.

Este vídeo mostra o comportamento disto no navegador.

![](/public/images/2018/11/bundle-size-async-toastify.gif)

Você pode encontrar mais detalhes de como isto foi implementado nesta [PR #645](https://github.com/getfider/fider/pull/645)

## Font Awesome e Tree Shaking

Tree Shaking é o processo de importar somente o código que vamos usar e descartar o resto. Isto é habilitado por padrão quando usamos Webpack no modo de produção.

A forma tradicionar de usar Font Awesome é importar um arquivo CSS externo (que por sua vez referência outras Fontes). Este CSS faz uma mapa de cada ícone para uma classe CSS. O resultado e que apesar de nossas aplicações usarem apenas os ícones A, B e C, estamos forçando os navegadores a baixarem mais de 600 ícones sem necessidade.

Depois de algum tempo procurando, achamos o **react-icons**, um pacote NPM que possuí todas os ícones do Font Awesome e de outras fontes também. Os ícones estã no format de SVG e são exportados como componentes React no format ES6 Module.

Isto significa que é possível importar apenas os ícones que precisamos, assim o webpack consegue remover todos os outros ícones que não são necessários. O resultado? **Nosso CSS ficou 68kB menor**. Sem contar que agora não é necessário fazer download de fontes externas também. Esta alteração foi o maior contribuinte para a redução do CSS no Fider.

Quer saber como fizemos? Veja esta [PR #631](https://github.com/getfider/fider/pull/631)

## Trocando grandes pacotes NPM por opções menores

> "NPM é como uma grande loja de lego, cheia de peças de montar disponíveis para você escolher. Você não paga pelo pacote que é instalado, mas seus usuários pagam pelos bytes que isto adicionado à sua aplicação. Escolha cuidadosamente." - @goenning

Novamente usando o Webpack Bundle Analyzer, descobrimos que o pacote `markdown-it` sozinho representava 40% do nosso vendor bundle. Decidimos então procurar no NPM por outra alternativa. O objetivo era achar um pacote que fosse menor, com desenvolvimento ativo e tivesse todas as funcionalidades que precisávamos.

Usamos bastante o [bundlephobia.com](https://bundlephobia.com/) para analisar o tamanho total dos pacotes NPM. Encontramos então o `marked`, que **reduziu nosso vendor bundle em 63kb** com uma alteração mínima no nosso código.

Está curioso? Veja esta [PR #643](https://github.com/getfider/fider/pull/643).

Você também pode comparar este dois pacotes no bundlephobia

- [https://bundlephobia.com/result?p=marked@0.5.2](https://bundlephobia.com/result?p=marked@0.5.2)
- [https://bundlephobia.com/result?p=markdown-it@8.4.2](https://bundlephobia.com/result?p=markdown-it@8.4.2)

Pense duas vezes antes de adicionar um pacote. Você realmente precisa dele? Será que não é possível implementar uma alternativa mais simples? Será que tem algum outro pacote que faz o mesmo trabalho com menos bytes? Caso contrário, você também pode optar por adicionar este pacote e carregá-lo sob demanda como fizemos com o react-toastify mencionado acima.

## Otimizando o bundle principal é crucial

Imagine que sua aplicação faz a divisão de codigo por rotas. A aplicação já está em produção e você faz uma alteraçao no componente `Dashboard`. Você acha que o Webpack gerará um novo bundle somente para o `Dashboard`?

Não é exatamente isto o que acontece.

O Webpack **sempre** vai regerar o bundle principal se qualquer outra parte da sua aplicação for alteradas. O motivo disto e que o bundle principal serve como um ponteiro para todos os outros bundles menores. Se o nome de um destes arquivos é alterado, o bundle principal tambem deve ser alterado com a nova referência dos outros bundles. Faz sentido, não é?

Então se seu bundle principal possui não apenas os ponteiros, mas também muito código como componentes básicos e outras funções, você está forçando o navegador a baixar tudo isto novamente.

Use o webpack bundle analyzer para entender o que faz parte do seu bundle principal e então aplique as técnicas apresentadas acima para remover (ou mover) o máximo possível de código.

## TSLib (apenas para usuários de TypeScript)

Quando compilamos código TypeScript para ES5, o compilador do TypeSript emite algunas funções de ajuda (helper functions) em cada arquivo JavaScript. Este processo garante que o código escrito em TypeScript funcionará em navegadores mais antigos que não suportam funcionalidades do ES6, como por exemplo, Classes e Generators.

Estas funções de ajuda são bem pequenas, mas a quantidade delas se multiplica baseado na quantidade de arquivos TypeScript de seu projeto. O Webpack não consegue fazer o tree shaking disto e o resultado é um bundle com dezenas (ou centenas) de funções dupicadas. Seu bundle acabe de ficar maior do que o necessário.

O próprio TypeScript possui uma solução para isto. Existe um pacote NPM chamado de **tslib** que contém todas estas funções de ajuda que o TypeScript precisa. Podemos instruir o compilador a usar as funções deste pacotes ao invés de emitir este código nos arquivos JavaScript.

Para configurar isto, basta instalar o pacote com **npm install tslib —save** e alterar o **tsconfig.json** e incluir a configuração **importHelpers: true**.

Isto é tudo!

A quantidade de bytes que isto reduzirá dependente da quantidade de arquivos TypeScript que usam código ES6, o que pode ser bastante em um projeto React.

## O próximo bilhão de usuários

você está pronto para o [próximo bilhão de usuários](https://developers.google.com/web/billions/)? Imagine todos os potenciais usuários de sua aplicação que hoje possuem dificuldade de usá-la em um dispositivo inferior ou com uma conexão de internet precária.

Reduzir o tamanho dos bundles possui um impacto direto no desempenho das aplicação e isto as torna mais acessíveis para todo mundo. Espero que este post te ajude nesta jornada.

Um abraço.