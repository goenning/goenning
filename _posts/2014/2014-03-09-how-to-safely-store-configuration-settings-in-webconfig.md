---
layout: post
title: How to safely store configuration settings in web.config
lang: en
tags: [C#, asp.net, web, security]
ref: how-to-safely-store-configuration-settings-in-webconfig
---

This is a very old topic (2005), but I've noticed that even today most developer does not know this or do not use it in some of their projects (that includes me!). What we are going to see here is required to keep our production environment safe.

The `web.config` file is commonly used to store sensitive data like password and confidential parameters. Ideally they should be hidden and not available in clear text for anyone that have access to this file. In fact this is the ideal place to store content of this nature, after all, we have sections for that, like `appSettings` and `connectionStrings`.

ASP.NET 2.0 has shipped with a very easy to use resource that can encrypt sections of the web.config. The best of all is that it is not necessary to change a single line of our code. All we need is to run an executable pointing to our site. It'll encrypt some web.config sections and our site will continue to work as usual.

Check for yourself. Go to a server where you have a running ASP.NET application that is published in IIS and run the follow command.

> aspnet_regiis.exe -pe “connectionStrings” -site “<your_site>” -app “<your_app>”

In case of a site without applications, just use `-app "/"`.

Open the `web.config` file and look at the `connectionStrings` section. It should be something like this:

![](/public/images/crypto-connectionstrings.png)

Cool, isn't it?

But there's a catch here. This command will encrypt an entire section. For `connectionStrings` this is really what most people should do, because it normally contains users and passwords. However this is not true for other sections like `appSettings`, where we mostly have application settings and (in rare cases) some confidential parameters as well.

The ideal solution is to encrypt only the confidential parameters, leaving the others in plain text. Fortunately this is easy to achieve. All we need is to create a new section, identical to `appSettings` and encrypt only this new section.

Our web.config would be like this:

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

We can now run the same encrypt command targeting only this new section.

> aspnet_regiis.exe -pe “secureAppSettings” -site “<your_site>” -app “<your_app>”

By using this technique we do not have to do anything to decrypt this parameter when we use it in our application, however it does change the way we access the parameters from this new section. The following class can be used to read the parameters.

~~~csharp
public static class SecureAppSettings
{
    public static string Get(string key)
    {
        NameValueCollection settings = ConfigurationManager.GetSection("secureAppSettings") as NameValueCollection;
        return settings[key];
    }
}

public class Program
{
    public static void Main(string[] args)
    {
        string password = SecureAppSettings.Get("WebServicePassword");
    }
}
~~~

What we saw here is simple and very useful.

From now on there is no excuse on leaving passwords in clear text anymore.
