---
layout: post
title: Referências do React fortemente tipadas com TypeScript
lang: pt
tags: [typescript, react]
ref: strongly-typed-react-refs-with-typescript
---

Quem já trabalhou com React por algum tempo provavelmente já descobriu que [`refs`](https://facebook.github.io/react/docs/refs-and-the-dom.html) são bem fáceis de se trabalhar, 
porém começam a se tornar [inflexíveis](http://stackoverflow.com/questions/29503213/use-state-or-refs-in-react-js-form-components) a partir de certo ponto.

De qualquer forma, `refs` estão disponíveis e funcionam muito bem. Então por que não utilizadas, correto?

Quando trabalhamos com TypeScript é uma pratica comum ter tudo o mais tipado possível.

Infelizmente `refs` é tipado como `[key: string]: ReactInstance`, o que nos forca a digitar `this.refs["myInput]`. Definitivamente não é o que queremos.

### Usando referências como callbacks

`ref` como callback é a prática recomendada atualmente e a solução com TypeScript é bem simples.

```typescript
import * as React from "react";

export class CustomTextInput extends React.Component<{}, {}> {
  private textInput: HTMLInputElement;

  constructor() {
    super();
    this.focus = this.focus.bind(this);
  }

  public focus() {
    this.textInput.focus();
  }

  public render() {
    return <div>
              <input
                type="text"
                ref={(ref) => this.textInput = ref}
              />
              <input
                type="button"
                value="Focus the text input"
                onClick={this.focus}
              />
           </div>;
  }
}


export class AutoFocusTextInput extends React.Component<{}, {}> {
  private input: CustomTextInput;

  constructor() {
    super();
  }

  protected componentDidMount() {
    this.input.focus();
  }

  public render() {
    return <div>
              <CustomTextInput
                ref={(ref) => this.input = ref}
              />
           </div>;
  }
}

```

Este exemplo é **exatamente** igual ao da [documentação oficial](https://facebook.github.io/react/docs/refs-and-the-dom.html).
A única pequena diferença é que adicionamentos uma propriedade tipada em nossa classe `private textInput: HTMLInputElement;`.
Com isto agora é possível digitarmos `this.input.value`, `this.input.focus()`, `this.input.maxLength` e assim por diante, sem erros de compilação.

### Usando referências como string

[Referências como string eventualmente serão depreciadas](https://github.com/facebook/react/issues/6250). 
Mas caso você ainda esteja utilizando e não quer migrar para callbacks, é também muito fácil adicionar tipagem à elas.

```typescript
import * as React from "react";

export class CustomTextInput extends React.Component<{}, {}> {
  public refs: {
    textInput: HTMLInputElement;
  };

  constructor() {
    super();
    this.focus = this.focus.bind(this);
  }

  public focus() {
    this.refs.textInput.focus();
  }

  public render() {
    return <div>
              <input
                type="text"
                ref="textInput"
              />
              <input
                type="button"
                value="Focus the text input"
                onClick={this.focus}
              />
           </div>;
  }
}
```

Estamos simplesmente sobrescrevendo a propriedade `refs` para ser fortemente tipada. Sim, isto é tudo.

A única diferença entre este exemplo e o de cima é que neste estamos usando a propriedade padrão do React `refs`, enquanto no outro estamos definindo uma nova. 

Seja qual for sua preferência, apenas lembre-se de sempre "tipar" seus componentes React tanto quanto possível.