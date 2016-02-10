---
layout: post
title: 'New project: SharpSapRfc'
comments: true
tags: [sap, abap, rfc, sharpsaprfc]
---

If you are new to SAP RFC, I suggest you to first read the post [Connecting the world to SAP with RFC](/2014/06/13/connecting-the-world-to-sap-with-rfc).

## SharpSapRfc

I was impressed by how SAP NCo's API is easy to understand, learn and code. All methods follow a well designed coding standard, with a strong oriented object structured, with well designed error messages, among other things. Even thought it is an excellent tool, I decided to create this library to make things even easier. It is possible?

The project is open-source and is hosted at [goenning/SharpSapRfc](https://github.com/goenning/SharpSapRfc). There are a few examples in the home page as well as many other examples in the tests project.

The great addition of this project to your codebase is the auto mapping between structures and tables to your C# classes.

I'll leave a comparative example between both projects bellow.

#### SAP NCo
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

#### SharpSapRfc
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

Hard to find any difference? That's because this example is using a simples string as input/output. The major advantage comes when you have POCO classes.

~~~csharp
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

> `Z_SSRT_GET_ALL_CUSTOMERS` is a function that returns a table category.

As you can see, we don't even need to map all columns. We do need, however, to map columns in which the name is different from the remote name.

### Update

In version `1.0.2` I've add support to `DateTime` properties (that was Slaski's souggestion through issue [#10](https://github.com/goenning/SharpSapRfc/issues/10)).

I've noticed that SAP's date and hour fields are always splited in two columns, while in C# we commonly use DateTime type which contains both information.

So I've deiced to implement the parse of these types to combine both remote fields into a simples property. All we need to do is map both fields' name, just like this example.

~~~csharp
public class ZMyClass
{
    [RfcStructureField("DATUM", "UZEIT")]
    public DateTime DateTime { get; set; }
}
~~~

So, did you guys like it?
Feel free to open an issue with suggestions, I'm open to it.

Cheers!