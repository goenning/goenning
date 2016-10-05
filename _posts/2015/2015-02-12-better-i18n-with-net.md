---
layout: post
title: Better i18n with .Net
comments: true
lang: en
tags: [.net, i18n]
ref: better-i18n-with-net
---

.Net Framework ships with a simple — yet powerful — translation feature. It's all about those resources files (.resx) that can be created and edited inside Visual Studio. Nothing is so good that cannot be enhanced, is that correct?

I have a helper class that I commonly use in projects that needs i18n that helps save a lot when coding. Instead of implicit calling the resources classes and it's attributes, I've created an extension method for both `String` and `Enum` that does the necessary resource lookup and translation.

The usage is very straight forward. You have a string variable containing some text. When you call the `i18n()` extension method on this variable, it will lookup in the correct resource file for a translation using the current thread culture. If it does not find a translation for it, the original text is returned.

The failover strategy can be changed by using the interface `II18nLookupFailedStrategy`. With this interface it is possible to define what to do when the lookup for the translation fails. I've write three implementations for this interface that came to mind.

1. **IgnoreI18nLookupFailedStrategy**: Does nothing. *This is the default strategy.*
2. **FormatTextI18nLookupFailedStrategy**: If it does not find a translation, a text is prepended or appended to the original text. The idea is to add a text that clearly tells the user/programmer that a translation is missing and it is necessary to take an action. Eg.: *"TRANSLATION IS MISSING!!! -> Hello <- DO IT NOW"*
3. **ThrowExceptionI18nLookupFailedStrategy**: This is self-explanatory. The most rigorous strategy that forces the programmer to translate it immediately or the project won't run.

Just take a look at the following example.

~~~csharp
I18nConfig.SetResources(
    typeof(StringResources),
    typeof(EnumResources)
);

I18nConfig.LookupFailedStrategy = new ThrowExceptionI18nLookupFailedStrategy();

"Click here".i18n(); //Looks for 'Click_here' in the resources files
Gender.Male.i18n(); //Looks for 'Gender_Male' or 'Male' in the resources files
"FIRST_NAME".i18n(); //Looks for 'FIRST_NAME' in the resources files
~~~

Source code is available at [goenning/i18n-extension](https://github.com/goenning/i18n-extension).

Enjoy!