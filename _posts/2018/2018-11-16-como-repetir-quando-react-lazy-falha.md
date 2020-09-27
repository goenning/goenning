---
layout: post
title: Como repetir quando React lazy falha
lang: pt
tags: [react]
description: React 16.6 foi lançado e agora é mais fácil do que nunca fazer importacão dinâmica de componentes usando a nova funcão chamda lazy. Após alguns dias monitorando uma aplicação em produção que estava usando lazy, notamos errors no Browser onde alguns usuários não estavam conseguindo carregar os componentes dinâmicos. Aprenda aqui como mitigar este problema.
ref: retry-react-lazy-fails
---

React 16.6 foi lançado e agora é mais fácil do que nunca fazer importacão dinâmica de componentes usando a nova funcão chamda lazy.

Se você não sabe do que estou falando, recomendo começar por este post [https://pt-br.reactjs.org/blog/2018/10/23/react-v-16-6.html](https://pt-br.reactjs.org/blog/2018/10/23/react-v-16-6.html).


Após alguns dias monitorando uma aplicação em produção que estava usando lazy, notamos errors no Browser como este aqui:

```
Loading chunk 6 failed. (error: https://.../6.4e464a072cc0e5e27a07.js)
Loading CSS chunk 6 failed. (https://.../6.38a8cd5e9daba617fb66.css)	
```

**Por quê??!**

Para ser sincero, eu também sei não o motivo. No momento estou assumindo que possa ser algum problema de rede instável ou talvez uma conexão 3G muito lenta do lado do cliente, afinal isto ainda é algo bem comum!

Se este for o caso, como resolve este problema?

Podemos fazer aquilo que quase todo programador faz quando uma requisição HTTP falha: tente outra vez! 😄

**Como?**

Você sabia que a função **import(...)** que é usada no lazy retorna uma `Promise`? Isto basicamente significa que é possível encadear Promises como qualquer outra.

Abaixo há uma implementação de uma função retry usando promises.

```javascript
function retry(fn, retriesLeft = 5, interval = 1000) {
  return new Promise((resolve, reject) => {
    fn()
      .then(resolve)
      .catch((error) => {
        setTimeout(() => {
          if (retriesLeft === 1) {
            // reject('maximum retries exceeded');
            reject(error);
            return;
          }

          // Passing on "reject" is the important part
          retry(fn, retriesLeft - 1, interval).then(resolve, reject);
        }, interval);
      });
  });
}
```
> Source: [https://gist.github.com/briancavalier/842626](https://gist.github.com/briancavalier/842626)

Agora só precisamos usá-la em conjunto com a função lazy.

```javascript
// Lazy sem retry
const ProductList = lazy(() => import("./path/to/productlist"));

// Lazy com retry
const ProductList = lazy(() => retry(() => import("./path/to/productlist")));
```

Se o browser, por qualquer motivo que seja, não conseguir fazer o download deste módulo, 5 tentativas serão feitas a cada 1 segundo. Se todas as 5 tentativas falharem, então o erro mencionado no começo deste post é lançado.

Isto é tudo! 🎉

Abraços e até a próxima!