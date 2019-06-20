---
title: Armazenando configurações com segurança no web.config
layout: post
lang: pt
tags: [.net, segurança]
ref: how-to-safely-store-configuration-settings-in-webconfig
---
Este é um assunto bastante antigo (2005) mas que até hoje muita gente não conhece ou não aplica em seus projetos (inclusive eu), negligenciando a segurança no lugar da facilidade. O que vamos ver aqui é de suma importância para manter um ambiente seguro e livre de dor de cabeça.

O arquivo web.config é muito usado para armazenamento de dados sensíveis — como senhas e parâmetros sigilosos — que idealmente deveriam estar escondidos e não serem apresentados em texto puro. De fato este é o local ideal para guardamos conteúdo desta natureza, afinal temos seções dedicadas para isto, como o **appSettings** e **connectionStrings**.

O ASP.NET 2.0 trouxe consigo um recurso muito fácil para criptografar seções do web.config. O melhor de tudo é que não é necessário alterar nenhuma linha de código. Basta executar um comando que o arquivo é criptografado e o site continua funcionando. Mágica? Nada disto, faça um teste ai (mas antes faça um backup, só para garantir não é?). Apenas execute o seguinte código em um Prompt de Comando (com permissões administrativas), indicando um Site/App que esteja publicado no IIS.

> aspnet\_regiis.exe -pe &#8220;connectionStrings&#8221; -site &#8220;<seu\_site>&#8221; -app &#8220;<seu_app>&#8221;

No caso de um site sem aplicativos, use apenas **-app &#8220;/&#8221;**

Abra o arquivo web.config do site em questão e veja como ficou. Deve estar com um visual parecido com este:

![](/public/images/2014/08/crypto-connectionstrings.png)

Muito bacana, não é?

Existem um porém, pois este comando criptografa uma seção inteira. No caso da **connectionStrings** isto é desejável e recomendado, pois normalmente todas as entradas desta tag terão a informação de usuário e senha. Mas no caso da seção **appSettings** existe um pequeno problema. Muitos parâmetros que ficam neste local não são sensíveis e podem (e normalmente devem) ficar abertos, pois facilita o entendimento e a alteração. Mas há casos onde é necessário armazenar senhas nesta seção e não podemos deixá-las em texto puro. A solução é criar uma nova seção, idêntica ao **appSettings** e criptografar apenas a nova seção. 

Na prática o web.config fica assim:

~~~xml
<configuration>
  <configSections>
    <section name="secureAppSettings" type="System.Configuration.NameValueSectionHandler, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" />
  </configSections>

  <appSettings>
    <add key="webpages:Version" value="3.0.0.0" />
    <add key="webpages:Enabled" value="false" />
    <add key="ClientValidationEnabled" value="true" />
    <add key="UnobtrusiveJavaScriptEnabled" value="true" />
  </appSettings>

  <secureAppSettings>
    <add key="WebServicePassword" value="d2f!H@7xz" />
  </secureAppSettings>
</configuration>
~~~

Com isto podemos criptografar apenas a seção **secureAppSettings** usando o mesmo comando acima.

> aspnet\_regiis.exe -pe &#8220;secureAppSettings&#8221; -site &#8220;<seu\_site>&#8221; -app &#8220;<seu_app>&#8221;

Usando esta técnica também não precisamos fazer o trabalho de descriptografar, o próprio ASP.NET já faz isto. Só muda um pouco o código C# que usa esta seção.
  
A classe abaixo pode ser usada para ler os dados da nova seção.

~~~csharp
public static class SecureAppSettings
{
    public static string Get(string key)
    {
        NameValueCollection settings = ConfigurationManager.GetSection("secureAppSettings") as NameValueCollection;
        return settings[key];
    }
}
~~~

Vimos que é bastante simples e muito útil. Agora não há mais desculpa para deixarmos as senhas abertas no **web.config**.
  
Quem quiser se aprofundar mais no assunto pode procurar por dois tópicos em específico:

  1. Utilizar DPAPI ou RSA na criptografia.
  2. Gerar e exportar a chave privada para que a mesma chave possa ser usada em diferentes servidores (web farm)

Abraço!