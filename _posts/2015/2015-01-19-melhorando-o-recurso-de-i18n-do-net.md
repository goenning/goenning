---
title: Melhorando o recurso de i18n do .Net
lang: pt
layout: post
tags: [.net]
ref: better-i18n-with-net
---

O Framework .Net já vem com um simples — porém poderoso — sistema de tradução. São os arquivos resources (.resx) criados e manipulados dentro do próprio Visual Studio. Mas nada é tão bom que não possa ser melhorado, correto?

Tenho uma classe que costumo usar nos projetos com internacionalização que me ajuda bastante na hora de codificar. Ao invés de ter que ficar chamando implicitamente as classes de resource e seus atributos, eu crio um método de extensão na string e na enumeração e que este se encarrega de traduzir quando necessário.

O funcionamento é bem simples. Tenho por exemplo uma variável que tem o texto qualquer. Se utilizarmos o método i18n() nesta variável, será feito uma busca nos arquivos de resource na tentativa de se traduzir este texto. Caso encontre uma tradução usando o idioma atual da Thread, o texto traduzido é retornado, caso contrário, o texto original é mantido. 

Aproveitei a escrita deste post para dar uma incrementada na solução. Utilizei a técnica de polimorfismo e criei uma interface chamada **II18nLookupFailedStrategy**. Esta interface determina o que o método **i18n** deve fazer caso não encontre uma tradução. Escrevi três implementações que me vieram à cabeça.

  * **1. IgnoreI18nLookupFailedStrategy:** Não faz nada. Se não achar tradução, simplesmente não traduz. Esta é a estratégia padrão caso não seja alterada.
  * **2. FormatTextI18nLookupFailedStrategy:** Se não achar uma tradução, adiciona um texto antes e outro depois do conteúdo a ser traduzido. A ideia é que seja adicionar algum texto escandaloso para que o programador veja na tela que está faltando tradução em um texto, mas que não impede seu funcionamento.
  * **3. ThrowExceptionI18nLookupFailedStrategy:** Esta é a mais rigorosa de todas, se não achar a tradução uma Exception é lançada, forçando o programador a traduzi-la imediatamente.

Abaixo está alguns exemplos de uso do método e como mudar a estratégia de falha.

~~~csharp
I18nConfig.SetResources(
    typeof(StringResources), 
    typeof(EnumResources)
);

I18nConfig.LookupFailedStrategy = new ThrowExceptionI18nLookupFailedStrategy();
"Click here".i18n(); //Busca por 'Click_here' nos arquivos de Resource
Gender.Male.i18n(); //Busca por 'Gender_Male' ou 'Male' nos arquivos de Resource
"FIRST_NAME".i18n();
~~~

O código completo está publicado no GitHub, o endereço é <https://github.com/goenning/i18n-extension>

Isto é tudo. O post foi apenas para compartilhar este pequeno exercício de uso de métodos de extensão e polimorfismo que considero bastante útil e didático, principalmente a tradução de Enum.

Abraço!