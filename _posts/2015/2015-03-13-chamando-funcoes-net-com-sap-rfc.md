---
title: Chamando funções .NET com SAP RFC
layout: post
lang: pt
tags: [abap]
---
No post [Conectando o mundo externo ao SAP com o uso de RFC](/2015/03/13/conectando-o-mundo-externo-ao-sap-com-o-uso-de-rfc/) falamos sobre como fazer aplicações Java e .NET acessarem funções do SAP através do protocolo RFC.

Algo que me parece pouco comum — porém com grande potencial e utilidade — é a possibilidade de fazer um programa ABAP invocar métodos escritos em Java ou .NET usando este mesmo protocolo. Aprendemos no post acima que quando invocamos uma função através de **CALL FUNCTION** é possível definir um destinatário RFC. Qualquer servidor de aplicação ABAP pode ser cadastrado como um destinatários RFC — e é por isto que é possível fazer chamadas remotas entre servidores ABAP. As bibliotecas **JCo** e **NCo** permitem que nós criemos um servidor RFC que também pode ser usado como um destinatário.

Neste post vamos ver um passo a passo de como fazer isto. Vamos usar como exemplo um programa ABAP que lista os repositórios públicos do GitHub de um usuário através de um método escrito em .NET.

![](/public/images/2015/03/sap-github-nco.png)

## Criando um destinatário RFC

O primeiro passo é criar uma nova entrada na transação SM59 com o tipo de conexão **T** (TCP/IP Connection).
  
No meu caso chamei o destinatário de DOTNET_SERVER, marquei a opção &#8220;Registered Server Program&#8221; e usei o mesmo nome no PROGRAM ID. 

Qual a diferença entre estes dois campos? O nome do destinatário é o nome usado no ABAP nas chamadas &#8216;CALL FUNCTION <nome-da-funcao> DESTINATION <DOTNET_SERVER>&#8217;, enquanto o PROGRAM ID é um token de correlação com o servidor que iremos criar em .NET. Voltaremos a falar dele mais a diante.

Informei também o host e o serviço do **[SAP Gateway](http://help.sap.com/saphelp_nw73/helpdata/en/31/42f34a7cab4cb586177f85a0cf6780/frameset.htm)** presente em meu ambiente MiniSAP. Toda instância ABAP possui um Gateway que é responsável pela comunicação RFC. 

Veja como ficou o cadastro.

![](/public/images/2015/03/rfc-destination.png)

## Criar a função ABAP

Por mais que a função será executada somente no sistema remoto, precisamos entrar na transação SE37 e criar o esqueleto da função, isto é, informar quais serão os parâmetros de entrada e saída. O servidor remoto utiliza esta estrutura da função para gerar os metadados necessários para que o SAP Connector funcione. 

A nossa função vai se chamar **LIST\_GITHUB\_REPO**. Ela recebe o usuário por parâmetro de entrada e retornar uma tabela de repositórios. Não é necessário inserir código fonte. Veja como ficou:

![](/public/images/2015/03/fm_gh_repo.png)

**Obs.:** Não esquecer de habilitar a opção **Remote-Enable Function** na primeira aba.

Esta é a estrutura e a categoria de tabela que a função retorna:
  
![](/public/images/2015/03/zstr_repo.png)

## Criando nosso servidor RFC em .NET

O ideal é que seja criado um projeto do tipo **Windows Service** para que este possa ser instalado como serviço e iniciado automaticamente junto com o sistema operacional. Como isto aqui é apenas uma demonstração, vou criar um projeto Console que é mais fácil de executar e depurar caso necessário.

O primeiro passo é adicionar uma referência ao SAP NCo. Para isto estou usando o pacote [SAPNCo.x86](https://www.nuget.org/packages/SAPNCo.x86/) do NuGet (também está disponível a versão [x64](https://www.nuget.org/packages/SAPNCo.x64/) para quem se interessar). Também vamos precisar do pacote [Octokit](https://www.nuget.org/packages/Octokit/) para fazer a consulta no GitHub.

Por baixo dos panos o que o Octokit faz é chamar a API pública do GitHub. Veja um exemplo dela em ação: <https://api.github.com/users/goenning/repos>

No arquivo de configurações precisamos registrar dois elementos: **Destination** e **Server**. 

  * O **Destination** contém as credenciais e parâmetros de acesso ao SAP para coleta de metadados. Esta configuração é mesma usada no exemplo anterior quando nosso aplicativo era apenas Client.
  * O **Server** possui as configurações que nosso servidor vai usar para se registrar no Gateway de destino.

O meu arquivo de configuração ficou assim.

~~~xml
<configuration>
  <configSections>
    <sectionGroup name="SAP.Middleware.Connector">
      <sectionGroup name="ClientSettings">
        <section name="DestinationConfiguration" type="SAP.Middleware.Connector.RfcDestinationConfiguration, sapnco"/>
      </sectionGroup>
      <sectionGroup name="ServerSettings">
        <section name="ServerConfiguration" type="SAP.Middleware.Connector.RfcServerConfiguration, sapnco"/>
      </sectionGroup>
    </sectionGroup>
  </configSections>

  <SAP.Middleware.Connector>

    <ClientSettings>
      <DestinationConfiguration>
        <destinations>
          <add NAME="TST" USER="bcuser" PASSWD="sapadmin2" CLIENT="001"
               LANG="EN" ASHOST="sap-vm" SYSNR="00" />
        </destinations>
      </DestinationConfiguration>
    </ClientSettings>

    <ServerSettings>
      <ServerConfiguration>
        <servers>
          <add NAME="DOTNET_SERVER" GWHOST="sap-vm" GWSERV="sapgw00" PROGRAM_ID="DOTNET_SERVER" REPOSITORY_DESTINATION="TST" REG_COUNT="1" />
        </servers>
      </ServerConfiguration>
    </ServerSettings>

  </SAP.Middleware.Connector>

  <startup>
    <supportedRuntime version="v4.0" sku=".NETFramework,Version=v4.5" />
  </startup>
</configuration>
~~~

Lembram que falei do PROGRAM ID? Vejam que aqui estamos usando os mesmos valores que foram informamos na SM59 para os três parâmetros: **PROGRAM_ID**, **GWHOST** e **GWSERV**.

Vamos precisar de apenas duas classes:

  * **Program:** O ponto de início da aplicação console. Responsável pela criação e inicialização do servidor RFC.
  * **MyRfcHandler:** Classe que irá conter os métodos que são invocados remotamente.

O código delas ficou assim:

~~~csharp
public class Program
{
    public static void Main(string[] args)
    {
        Type[] handlers = new Type[] { typeof(MyRfcHandler) };
        RfcServer server = RfcServerManager.GetServer("DOTNET_SERVER", handlers);
        server.RfcServerError += RfcServer_OnError;
        server.Start();
        Console.WriteLine("Server running. Press key to stop…");
        Console.ReadLine();
        server.Shutdown(true);
    }

    public static void RfcServer_OnError(object server, RfcServerErrorEventArgs errorEventData)
    {
        Console.WriteLine("Error: " + errorEventData.Error.Message);
    }
}
~~~

No caso de um projeto Windows Service este código deveria estar no método **OnStart** da classe de serviço.

~~~csharp
public class MyRfcHandler
{ 
    [RfcServerFunction(Name="LIST_GITHUB_REPO")]
    public static void StfcConnection(RfcServerContext myContext, IRfcFunction function)
    {
        var github = new GitHubClient(new ProductHeaderValue("NCoSampleServer"));

        string username = function.GetString("USER");
        var task = github.Repository.GetAllForUser(username);
        task.Wait();

        IRfcTable table = function.GetTable("REPOS");
        RfcStructureMetadata structure = table.Metadata.LineType;public
        foreach(Repository repo in task.Result)
        {
            IRfcStructure row = structure.CreateStructure();
            row.SetValue("ID", repo.Id);
            row.SetValue("NAME", repo.Name);
            row.SetValue("URL", repo.HtmlUrl);
            table.Append(row);
        }
    }
}
~~~

A classe **Program** é bem simples. Criamos uma instância de RfcServer usando os parâmetros definidos no arquivo de configuração e informamos quais as classes que irão responder às chamadas remotas. Neste caso só temos uma, a **MyRfcHandler**, mas poderiam haver outras. Todo método remoto deve ser marcado como **static void** e possui um atributo **[RfcServerFunction]** onde informamos o nome da função que criamos no ABAP. É baseado nestas configurações que o NCo sabe qual o método .NET que deve ser invocado quando receber uma chamada remota.

Esta função utiliza o Octokit para fazer a consulta no GitHub e preencher a tabela de retorno REPOS com os dados obtidos. Até o momento só haviamos visto como trabalhar com tipos primitivos no NCo. Neste exemplo tive que usar tabelas, mas como você pode ver no código acima não é nenhum bicho de 7 cabeças. O segredo está em conhecer as classes **IRfcTable**, **IRfcStructure** e **RfcStructureMetadata**.

Já estamos prontos para colocar nosso servidor RFC para funcionar, basta iniciar executar a aplicação.

## registration of tp DOTNET_SERVER from host <sua-maquina> not allowed

Caso você receba a mensagem de erro acima, não se apavore, pois isto é normal.
  
O SAP Gateway possui um controle de segurança onde somente hosts autorizados podem ser registradas como servidores externos.

Para adicionar nosso host nesta lista acesse a transação **SMGW -> Goto -> Parameters -> Display**.
  
Procure pelos parâmetros **gw/sec_info** e **gw/reg_info** e veja qual o caminho e nome do arquivo de cada um deles. Se for uma instalação nova é muito provável que estes arquivos nem existam ainda. Se este for o caso, crie-os primeiro.

Os meus arquivos ficaram assim:

secinfo.DAT

~~~
#VERSION=2
P TP=DOTNET_SERVER
~~~

reginfo.DAT

~~~
#VERSION=2
P HOST=NOTE-OENNING USER=BCUSER TP=DOTNET_SERVER
~~~

Substitua o HOST, USER e TP pelos valores que você está usando.
  
Recomendo este [link do help.sap](http://help.sap.com/saphelp_nw73/helpdata/en/e2/16d0427a2440fc8bfc25e786b8e11c/content.htm) para maiores informações sobre estas configurações. 

Feito isto acesso o menu **SMGW -> Goto -> Expert Functions -> External Security -> Reread**. Isto fará com que o SAP recarregue as configurações de segurança sem ter que reiniciá-lo por completo.
  
Agora sim podemos tentar executar nosso servidor novamente. Se der certo, a mensagem &#8216;Server running. Press key to stop…&#8217; deve aparecer no console.

Pronto, isto é tudo o que precisamos no lado .NET. Agora podemos voltar ao ABAP.

## Finalmente, a chamada da função

Vamos criar uma tela onde o usuário digita o nome do login do GitHub que ele gostaria de ver os repositórios. Usamos nosso servidor .NET para fazer a consulta e então exibimos os repositórios na tela. O código fica assim:

~~~
REPORT zrfc_list_github_repo.

PARAMETERS: p_user TYPE syuname OBLIGATORY.

START-OF-SELECTION.
  DATA: lt_repos TYPE ztbl_gh_repo.
  FIELD-SYMBOLS: <fs_repo> TYPE zstr_gh_repo.

  CALL FUNCTION 'LIST_GITHUB_REPO'
    DESTINATION 'DOTNET_SERVER'
    EXPORTING
      user  = p_user
    IMPORTING
      repos = lt_repos.

  LOOP AT lt_repos ASSIGNING <fs_repo>.
    WRITE: / <fs_repo>-id, 16 <fs_repo>-name , 50 <fs_repo>-url.
  ENDLOOP.
~~~

Ao executar o código, o resultado é este:

![](/public/images/2015/03/rfc_dotnet_result.png)

Pronto, funcionou! Veja que bacana, conseguimos integrar nosso SAP com o GitHub.

Já parou para pensar quanta coisa legal podemos fazer com isto? Principalmente em se tratando de integrações com sistemas externos como Correios, Serasa, Facebook, Twitter, Sefaz dentre outros serviços. Mas sem dúvida alguma a grande utilidade disto está na comunicação do SAP com sistema legados ou banco de dados externos. Vale a pena ressaltar que a SAP possui uma ferramenta criada exclusivamente para integração baseada em mensageria, o chamado  **SAP Process Orchestration** (antigo **Process Integration** ou **Exchange Infrastructure**). Vai de cada projeto ou cenário optar por escolher entre um ou outro. Uma das vantagens do NCo/JCo é o baixo custo, pois enquanto o Process Orchestration possui uma licença independente, a RFC vem de &#8216;grátis&#8217; no SAP.

Abraços!