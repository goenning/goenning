---
layout: post
title: Como evitar o caos de caminho relativo em projetos JavaScript / TypeScript
lang: pt
tags: [typescript, javascript]
ref: how-to-avoid-relative-path-hell-javascript-typescript-projects
description: Caso na importação de módulos usando caminho relativo é algo bem comum em grandes projetos JavaScript/TypeScript. Aprenda aqui como usar Webpack para resolver este problema.
---

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Thanks to <a href="https://twitter.com/hashtag/typescript?src=hash">#typescript</a> and <a href="https://twitter.com/hashtag/webpack?src=hash">#webpack</a> I can now freely move my components without having to deal with relative path hell :) <a href="https://t.co/q257QPoylI">pic.twitter.com/q257QPoylI</a></p>&mdash; Guilherme Oenning (@goenning) <a href="https://twitter.com/goenning/status/880884293500850176">June 30, 2017</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

Quem já trabalhou em um projeto JavaScript já deve estar familiar com o problema de importação de módulos usando caminho relativo (também conhecido como **relative path hell**).

Quando um módulo local importa outro módulo local, o caminho do módulo deve ser relativo, assim como estes dois exemplos.

```typescript
import { DefaultRenderer } from '../../../renderers'
import { PayPalClient } from '../../services/PayPal'
```

Isto pode ser um problema quando o tamanho do código aumenta, pois é necessário saber exatamente em que diretório estão os dois módulos, tornando a refatoração e a legibilidade muito mais complicada.

Onde é que `../../../renderers` está localizada? Você consegue facilmente encontrá-lo? Um ou dois diretórios acima tudo bem, porém mais que isto fica compicado.

Com relação à refatoração, quando qualquer **módulo** é movido para cima ou para baixo na árquivo de arquivos, é necessário alterar o caminho relativo dos consumidores e também dos módulos importantes. Nada legal, não é?

## Webpack ao resgate!

Webpack pode ser usado para criar um alias (apelido) para um determinado diretório, então ao invés de utilizar um caminho relatóvio, é possível iniciar a importação através do apelido.

O exempo anterior pode ser re-escrito assim:

```typescript
import { DefaultRenderer } from '@app/renderers'
import { PayPalClient } from '@app/services/PayPal'
```

Melhor, não é? Não é preciso se importar em que diretório o módulo consumidor está, apenas onde o módulo a ser importado se encontra após o apelido. A alteração no webpack acontece no arquivo `webpack.config.js`, basta adicionar a propriedade `resolve.alias`.

```javascript
resolve: {
  alias: {
    '@app': path.resolve(__dirname, 'src/')
  }
}
```

Neste exemplo tudo o que começar com `@app` será carregado de `src/`. Eu gosto de chamar de `@app`, pois fica fácil de lembrar que você está importanto algo de seu próprio projeto e não de um pacote NPM.

O benefício é que neste caso não é relevante em que diretório onde o módulo consumidor está, apenas o módulo a ser importado.

Vale a pena mencionar que isto também funciona com arquivos SCSS, mas é necessário adicionar o prefixo **~**.

```scss
@import '~@app/styles/variables.scss';
```

## Você usa TypeScript? Sem problemas, também temos uma solução para você

Caso esteja usando TypeScript e tentar implementar o alias do webpack, logo notará que o compilador não consegue encontrar os módulos, afinal o TypeScript não faz noção da configuração que fizemos no Webpack.

Existe uma configuração no compilador semelhantes ao do Wepback

```json
{
    "compilerOptions": {
        "baseUrl": "./src",
        "paths": {
            "@app/*": [
                "*"
            ]
        }
    }
}
```

E isto é tudo o que você precisa. O compilador agora sabe onde encontrar a definição de tipos de qualquer módulo que inicie com `@app`.

*Note que estas configurações NÃO são mutuamente exclusivas, se você utilizar TypeScript, ambas as configurações são necessárias.*

Espero que isto ajude a manter o seu código mais legível!

Um abraço!