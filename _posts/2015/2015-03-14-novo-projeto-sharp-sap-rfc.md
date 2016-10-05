---
title: 'Novo projeto: Sharp SAP RFC'
layout: post
comments: true
lang: pt
tags: [abap, github, opensource, sharpsaprfc]
ref: new-project-sharp-sap-rfc
---

No artigo [Conectando o mundo externo ao SAP com o uso de RFC](/2015/03/13/conectando-o-mundo-externo-ao-sap-com-o-uso-de-rfc/) falei um pouco sobre as chamadas de RFC usando .Net e gostei tanto de estudar o SAP NCo que eu acabei criando uma nova biblioteca.

## Sharp SAP RFC

Fiquei impressionado com a API do SAP NCo. É muito fácil de aprender, entender e programar. Os métodos possui uma nomenclatura muito explicativa, possui estrutura muito forte de orientação a objetos, mensagens de erro bem definidas, entre outros. Mesma sendo uma ferramenta muito boa, decidi criar uma biblioteca para facilitar ainda mais o seu uso.

O projeto está no <https://github.com/goenning/SharpSapRfc> e há alguns exemplos no README e muitos outros no projeto de testes.
  
A grande adição que este projeto traz é o mapeamento entre as estruturas e tabelas remotas para classes e propriedades do C#.

Deixo aqui um comparativo entre a API do SAP NCo e do SharpSapRfc.

~~~csharp
RfcDestination destination = RfcDestinationManager.GetDestination("TST");
RfcRepository repository = destination.Repository;
IRfcFunction function = repository.CreateFunction("Z_GET_SCAR");
function.SetValue("I_CARRID", "ZZZ");
try
{
    function.Invoke(destination);
    string name = function.GetString("E_CARRNAME");
    Console.WriteLine(name);
}
catch (RfcAbapException ex)
{
    if (ex.Key == "CARR_NOT_FOUND")
        Console.WriteLine("Não foi encontrado uma Cia. Aérea com o código informado.");
}
~~~

~~~csharp
using (SapRfcConnection conn = new SapRfcConnection("TST"))
{
    try
    {
        var result = conn.ExecuteFunction("Z_GET_SCAR", new {
            I_CARRID = "ZZZ"
        });

        var name = result.GetOutput<string>("E_CARRNAME");
        Console.WriteLine(name);
    }
    catch (RfcAbapException ex)
    {
        if (ex.Key == "CARR_NOT_FOUND")
            Console.WriteLine("Não foi encontrado uma Cia. Aérea com o código informado.");
    }
}
~~~

Sim, não parece ter muita diferença. Isto porque estamos usando apenas objetos simples. A grande vantagem vem com o uso de classes. Veja um exemplo.
  
**Z\_SSRT\_GET\_ALL\_CUSTOMERS** é uma função que retorna uma categoria de tabela.

~~~csharp
using (SapRfcConnection conn = new SapRfcConnection("TST"))
{
    var result = conn.ExecuteFunction("Z_SSRT_GET_ALL_CUSTOMERS");
    var customers = result.GetTable<ZCustomer>("t_customers");
    foreach (ZCustomer cust in customers) 
    {
        //Do anything.
    }
}

public class ZCustomer
{
    public int Id { get; set; }
    public string Name { get; set; }

    [RfcStructureField("ACTIVE")]
    public bool IsActive { get; set; }

    public int Age { get; set; }
}
~~~

Veja que nem precisamos mapear todas as colunas. Somente as que o nome da propriedade é diferente do nome da propriedade da estrutura no SAP.

O pacote está disponível nas versões [x86](https://www.nuget.org/packages/SharpSapRfc.x86/) e [x64](https://www.nuget.org/packages/SharpSapRfc.x64/) no NuGet.

**Bônus**

Na versão (1.0.2) adicionei suporte para as propriedades do tipo DateTime (sugestão do [Slaski](https://github.com/Slaski)).
  
Acabei notando que no SAP os campos de data e hora são sempre separados, enquanto que no C# existe um tipo de dado que combina as duas informações.
  
Então implementei também uma funcionalidade que permite mapear dois campos remotos em uma única propriedade.
  
O mapeamento é bem simples, ficou assim:

~~~csharp
public class ZMyClass
{
    [RfcStructureField("DATUM", "UZEIT")]
    public DateTime DateTime { get; set; }
}
~~~

E então, gostaram? É útil?
  
Estou aberto à sugestões para melhorar a ferramenta.

Um abraço.