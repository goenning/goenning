---
title: Habilitando SAP ICF na instalação MiniSAP
layout: post
lang: pt
tags: [abap, icf, minisap, sap]
ref: how-to-enable-sap-icf-in-minisap-installation
---

Ao tentar acessar o serviço de Ping do ICF (Internet Communication Framework) através do endereço padrão (que é `<nome-maquina>:8000/sap/bc/ping?sap-client=001`), nos deparamos com um erro de página não encontrada. Se este for o caso, não entre em pânico, pois a solução é simples.

### O Ping (e a maioria dos outros serviços) está inativo por padrão

Entre na transação SICF e navegue na árvore **default_host/sap/bc/ping**. Note que o serviço está inativo. Cntão clique com o botão direito neste item e senta o dedo no **Activate Service**. Agora podemos testar novamente. Há um atalho clicando com o botão direito e seguindo pela opção **Test Service**. Não funcionou? Ótimo, continue lendo.

### Porta padrão &#8216;0&#8217;?

Por padrão a porta do ICF é 0 e isto não funciona corretamente. No meu caso o endereço de teste ficou assim **http://sap-vm:0/sap/bc/ping?sap-client=001**. Para resolver isto basta acessar o arquivo **C:usrsapNSPSYSprofileDEFAULT.PFL** e acrescentar estas duas linhas ao final.

> icm/host\_name\_full = <nome-seu-servidor>
  
> icm/server\_port\_0 = PROT=HTTP,PORT=8000,TIMEOUT=3600,PROCTIMEOUT=3600 

Feito isto, acesse o endereço `http://<nome-seu-servidor>:8000/sap/bc/ping?sap-client=001` e informe as credenciais de acesso. Se tudo ocorrer bem a seguinte mensagem deve aparecer na tela.

**Server reached successfully**

Ótimo!

Estas configurações foram necessárias para testar um recurso muito útil do SAP que será descrito nos próximos posts.

Até logo!