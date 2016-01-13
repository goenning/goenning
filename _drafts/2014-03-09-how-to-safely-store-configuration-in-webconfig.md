---
layout: post
title: How to safely store configuration in web.config
comments: true
---

This is a very old topic (2005), but I've noticed that even today most developer does not know this or do not use it in some of their projects (including me!). What we are going to see here is of great importance to keep our production environment safe.

The `web.config` file is commonly used to store sensitive data like password and confidential parameters. Ideally they should be hidden and not available in clear text for anyone that opens this file. In fact this is the ideal place to store content of this nature, after all, we have sections for that, like `appSettings` and `connectionStrings`.

ASP.NET 2.0 has shipped with a very easy to use resource that can crypt sections of the web.config. The best of all is that it is not necessary to change a single line of our code. All we need is to run an executable and point it to our web.config file and our site will continue to work as usual.

Check for yourself. Go to a server where you have a running ASP.NET application that is published in IIS and run the follow command.

    aspnet_regiis.exe -pe “connectionStrings” -site “<your_site>” -app “<your_app>”

In case of a site without applications, just use `-app "/"`.
Open the `web.config` file and look at the `connectionStrings` section. It should be something like this:

![](/public/images/crypto-connectionstrings.png)

---

Este é um assunto bastante antigo (2005) mas que até hoje muita gente não conhece ou não aplica em seus projetos (inclusive eu), negligenciando a segurança no lugar da facilidade. O que vamos ver aqui é de suma importância para manter um ambiente seguro e livre de dor de cabeça.

O arquivo web.config é muito usado para armazenamento de dados sensíveis — como senhas e parâmetros sigilosos — que idealmente deveriam estar escondidos e não serem apresentados em texto puro. De fato este é o local ideal para guardamos conteúdo desta natureza, afinal temos seções dedicadas para isto, como o appSettings e connectionStrings.

O ASP.NET 2.0 trouxe consigo um recurso muito fácil para criptografar seções do web.config. O melhor de tudo é que não é necessário alterar nenhuma linha de código. Basta executar um comando que o arquivo é criptografado e o site continua funcionando. Mágica? Nada disto, faça um teste ai (mas antes faça um backup, só para garantir não é?). Apenas execute o seguinte código em um Prompt de Comando (com permissões administrativas), indicando um Site/App que esteja publicado no IIS.

    aspnet_regiis.exe -pe “connectionStrings” -site “<seu_site>” -app “<seu_app>”
No caso de um site sem aplicativos, use apenas -app “/”

Abra o arquivo web.config do site em questão e veja como ficou. Deve estar com um visual parecido com este:

![My helpful screenshot](/public/images/crypto-connectionstrings.png)

Muito bacana, não é?

Existem um porém, pois este comando criptografa uma seção inteira. No caso da connectionStrings isto é desejável e recomendado, pois normalmente todas as entradas desta tag terão a informação de usuário e senha. Mas no caso da seção appSettings existe um pequeno problema. Muitos parâmetros que ficam neste local não são sensíveis e podem (e normalmente devem) ficar abertos, pois facilita o entendimento e a alteração. Mas há casos onde é necessário armazenar senhas nesta seção e não podemos deixá-las em texto puro. A solução é criar uma nova seção, idêntica ao appSettings e criptografar apenas a nova seção.

Na prática o web.config fica assim:

```xml
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
    <add key="Password" value="d2f!H@7xz" />
  </secureAppSettings>
</configuration>
```

Com isto podemos criptografar apenas a seção secureAppSettings usando o mesmo comando acima.

aspnet_regiis.exe -pe “secureAppSettings” -site “<seu_site>” -app “<seu_app>”
Usando esta técnica também não precisamos fazer o trabalho de descriptografar, o próprio ASP.NET já faz isto. Só muda um pouco o código C# que usa esta seção.
A classe abaixo pode ser usada para ler os dados da nova seção.


```csharp
public static class SecureAppSettings
{
    public static string Get(string key)
    {
        NameValueCollection settings = ConfigurationManager.GetSection("secureAppSettings") as NameValueCollection;
        return settings[key];
    }
}
```
Vimos que é bastante simples e muito útil. Agora não há mais desculpa para deixarmos as senhas abertas no web.config.
Quem quiser se aprofundar mais no assunto pode procurar por dois tópicos em específico:

Utilizar DPAPI ou RSA na criptografia.
Gerar e exportar a chave privada para que a mesma chave possa ser usada em diferentes servidores (web farm)
Abraço!