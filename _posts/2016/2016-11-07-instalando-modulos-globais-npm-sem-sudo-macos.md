---
layout: post
title: Instalando modulos globais do NPM sem sudo no macOS / Linux
comments: true
lang: pt
tags: [npm, macOS, node.js]
---

Quando instalamos um módulo global do NPM os arquivos são colocados dentro da pasta `/usr/local`.
O problema é que esta pasta possui acesso de escrito restrita em ambiente Unix, fazendo com que tenhamos que usar `sudo` toda vez que precisamos instalar um novo módulo global.

Depois de muito tempo resolvi mudar minha estratégia e passei a instalar os módulos globais em outro local. 
Desta forma não preciso mais usar `sudo` e consigo ter mais controle sobre a organização dos meus diretórios, afinal o NPM estava enchendo meu `/usr/local/bin/` de executáveis.

Siga abaixo as instruções para fazer o mesmo em sem ambiente.

### 1. Limpeza (opcional)

Você pode começar limpando todos os módulos globais já instalados, afinal, você vai passar a utilizar uma nova pasta e é sempre bom não deixar sugeira. O comando `npm ls -g --depth=0` pode ser usado para verificar todos os pacotes globais instalados.

Você pode remover um por um, usando `sudo npm -g rm <nome>` ou então algo mais agressivo seria usar o seguinte conjunto de instruções.

> $ npm ls -gp --depth=0 | awk -F/ '/node_modules/ && !/\/npm$/ {print $NF}' | xargs npm -g rm

A explicação está contida no [Stack Overflow](http://stackoverflow.com/questions/9283472/command-to-remove-all-npm-modules-globally), mas basicamente ele irá varrer todos módulos globais e executar o `npm rm` para cada um.

### 2. Pasta padrão

Utilize o comando `npm config get prefix` para verificar que pasta o NPM está configurado para gravar novos módulos globais. Se nenhuma alteração foi realizada ainda, este comando deve imprimir `/usr/local`.

Optei por colocar todos os módulos globais dentro do Home do meu usuário, para isto criei uma pasta exclusiva para este objetivo.

> $ mkdir ~/.npm-global

### 3. Configurando a pasta no NPM

A maneira mais fácil de mudarmos esta pasta no NPM é utilizando a variável de ambiente `NPM_CONFIG_PREFIX`.

Para isto basta editar o arquivo `~/.bash_profile` e adicionar as seguintes linhas:

```bash
export NPM_CONFIG_PREFIX=~/.npm-global
export PATH=~/.npm-global/bin:$PATH 
```

Salve o arquivo e reinicie seu terminal (ou recarregue-o com `source ~/.bash_profile`).

Se tudo ocorrer bem, o comando `npm config get prefix` agora irá imprimir `/Users/oenning/.npm-global`.

### 4. Instalando pacotes sem sudo

Tudo pronto, agora basta instalar os pacotes sem o uso de `sudo`. Veja só.

```bash
oenning@MacBook-Pro ~ $ npm i typescript -g
/Users/oenning/.npm-global/bin/tsc -> /Users/oenning/.npm-global/lib/node_modules/typescript/bin/tsc
/Users/oenning/.npm-global/bin/tsserver -> /Users/oenning/.npm-global/lib/node_modules/typescript/bin/tsserver
/Users/oenning/.npm-global/lib
└── typescript@2.0.7 

oenning@MacBook-Pro ~ $ tsc -v
Version 2.0.7
oenning@MacBook-Pro ~ $ 
```