---
layout: post
title: Não use require para ler arquivos JSON
lang: pt
tags: [javascript, node.js]
ref: stop-reading-json-files-with-require
---

E lá está você, programando às 11 da noite, tentando fazer seu teste unitário ficar verde novamente. Você faz tudo o que é possível, adicionar `console.log` em todas as funções, mas você simplesmente `não consegue achar o motivo!`.

Por quê? Por que é que não funciona mais? Por quê?

Este era eu na noite passada.

![](/public/images/hate-programming.jpg)

Depois de perder 1 hora tentanto resolver o problema, eu provavalmente deveria tatuar isto or colocar na parede do escritório, mas resolvi pelo menos escrever este post.

### Lendo arquivos JSON com Node.js

JavaScript é ótimo quando trabalhamos com arquivos JSON. Não é necessário nenhuma biblioteca externa como [Json.NET](http://www.newtonsoft.com/json) ou [Gson](https://github.com/google/gson) para fazer o processamento de strings JSON em um objeto. E também não é necessário criar nenhuma classe que só será usada uma vez, algo bem comum em C# e Java.

Sempre usei `require()` para carregar arquivos JSON em uma variável. Mais ou menos assim:

`settings.json`

~~~json
{
  "name": "My Application Name",
  "tags": [
    "nodejs",
    "javascript"
  ]
}
~~~

`index.js`

~~~javascript
var settings = require('./settings.json')
console.log(settings.tags) // ['nodejs', 'javascript']
~~~

Fácil e limpo, certo?

Mas há um *efeito colateral* aqui. Algo que pode ser tanto `bom` quanto `ruim`, vai depender de sua aplicação.

No meu caso eu estava usando em um teste unitário e isto estava me causando mais dor de cabeça do que ajudando.

Indo direto ao ponto...

`require()` sempre fará o cache do módulo (ou arquivo, neste caso). A próxima ver que `require()` for invocado para ler o mesmo módulo, o seu conteúdo será restaurado do cache ao invés de ser lido novamente. Isto parece ótimo, porém...

Eu estava lendo o arquivo em cada caso de teste. O primeiro teste modificada o conteúdo, e por conta do cache, o conteúdo modificado também estava disponível no segundo caso de teste. Eu esperava que no segundo teste o conteúdo seria igual ao original, mas não era. O resultado? Um teste quebrado.

Para quem gosta de ver código, aqui está um exemplo hipotético.

~~~javascript
var expect = require('chai').expect
var fs = require('fs')

describe("Require", () => {
  it("should be able to change settings values", () => {
    var settings = require('./settings.json')
    settings.tags.push('v8')
    expect(settings.tags).to.be.deep.equal(["nodejs", "javascript","v8"])
  })

  it("should reload settings file", () => {
    var settings = require('./settings.json')
    expect(settings.tags).to.be.deep.equal(["nodejs", "javascript"])
  })
})
~~~

Talvez você espere que os dois testes passam, mas o segundo não vai. O segundo falhará pois `settings.tags` agora contém `"v8"`.

Há também outro problema com esta implementação. `require()` é síncrono. Como bons desenvolvedores node.js, sabemos o quanto I/O blocante é perigoso. Utilizar require para carregar módulos nativos ou pacotes NPM é OK, afinal, precisamos que seja síncrono e cacheado, mas com arquivos JSON a história é diferente.

A solução é fácil, use o módulo `fs` para ler arquivos JSON, assim como qualquer outro formato de arquivo.

Agora estou utilizando a segunda função.

~~~javascript
var readJson = (path, cb) => {
  fs.readFile(require.resolve(path), (err, data) => {
    if (err)
      cb(err)
    else
      cb(null, JSON.parse(data))
  })
}
~~~

E os testes podem ser re-escritos assim.

~~~javascript
describe("File System", () => {
  it("should be able to change settings values", (done) => {
    readJson('./settings.json', (err, settings) => {
      settings.tags.push('v8')
      expect(settings.tags).to.be.deep.equal(["nodejs", "javascript","v8"])
      done(err)
    })
  })

  it("should reload settings file", (done) => {
    readJson('./settings.json', (err, settings) => {
      expect(settings.tags).to.be.deep.equal(["nodejs", "javascript" ])
      done(err)
    })
  })
})
~~~

Sem cache, sem I/O blocante, testes verdes e o código continua fácil de ler :-)

Um abraço!
