---
layout: post
title: "Implementando o Padrão de Sessão por Requisição em Go"
lang: pt
tags: [go, web, sql]
description: Há um padrão de projeto muito comum em C# e Java, porém dificil de encontrar qualquer referência sobre em Go. O padrão é conhecido como Sessão por Requisição (Session per Request) e é particularmente útil para desacoplhar lógica de negócio do gerenciamente de transações com o banco de dados. Neste post vou falar sobre o que é, como implementar, as vantagens e desvantagens, assim como exemplos em Go.
ref: session-per-request-pattern-go
---

Antes de começar a codificar em Go, trabalhei em vários projetos usando C# e Java. Há um padrão de projeto que muito comum nestas linguagens, porém dificil de encontrar qualquer referência sobre em Go.

O padrão é conhecido como **Sessão por Requisição** e é particularmente útil para desacoplhar lógica de negócio do gerenciamente de transações com o banco de dados. Neste post vou falar sobre o que é, como implementar, as vantagens e desvantagens, assim como exemplos em Go.

A ideia por trás deste padrão é iniciar uma transação com o banco de dados no início de cada requisição HTTP e fazer o commit ou o rollback no final da requisição. Fazendo isto, evitamos ter que abrir e fechar as transação explicitamente para cada operação com o banco de dados, podemos simplesmente reutilizar a sessão que já foi criada para nós.

### A forma tradicional de gerenciar transações com o banco de dados

O código a seguir é um exemplo simples de como as transações geralmente são gerenciadas, não apenas em Go, mas como em qualquer outra linguagem.

```go
func index(w http.ResponseWriter, r *http.Request) {
	tx, err := db.Begin()
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Printf(err)
		return
	}

	var id int
	err = tx.QueryRow("SELECT id FROM pages WHERE url = $1", r.URL.Path).Scan(&id)
	if err != nil && err != sql.ErrNoRows {
		tx.Rollback()
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Println(err)
		return
	}

	if id == 0 {
		err := tx.QueryRow("INSERT INTO pages (url, visitors) VALUES ($1, 0) RETURNING id", r.URL.Path).Scan(&id)
		if err != nil {
			tx.Rollback()
			w.WriteHeader(http.StatusInternalServerError)
			fmt.Println(err)
			return
		}
	}

	_, err = tx.Exec("UPDATE pages SET visitors = visitors + 1 WHERE id = $1", id)
	if err != nil {
		tx.Rollback()
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Println(err)
		return
	}

	_, err = tx.Exec("INSERT INTO page_visitors (page_id, ip, datetime) VALUES ($1, $2, $3)", id, r.RemoteAddr, time.Now())
	if err != nil {
		tx.Rollback()
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Println(err)
		return
	}

	err = tx.Commit()
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Println(err)
		return
	}

	w.Write([]byte(fmt.Sprintf("Thanks for visiting '%s'", r.URL.Path)))
	return
}
```

Este código basicamente inicia uma transação no início da função e executa alguns comandos, se a qualquer momento algum erro acontecer, é feito um rollback da transação e uma resposta 500 é retornada ao cliente. Se tudo ocorrer bem, é feito o commit da transação e uma resposta 200 é retornada.

Neste exemplo acabamos misturando lógica de gerenciamente de transação com comandos pertinentes ao negócio da aplicação. O código também ficou um pouco maior devido à necessidade de tratar os erros. Imagine como isto pode ficar complicado se tivermos que fazer isto em várias outros lugares da aplicação.

### Aplicando o padrão

Veremos agora como podemos melhorar este código.

Como escrito anteriormente, este padrão consiste em mover a lógica de tratamento de transações para outra camada que deverá ser **executada antes e depois** dos HTTP handlers. Em Go – assim como em outras linguagens – isto pode ser feito com HTTP middlewares.

Vamos criar então um middleware que abre a transação antes do handler ser executado. Com base no resultado do Handler, o middleware fará o commit ou rollback da transação. Sendo assim, removemos toda a necessidade de gerenciar transações de dentro do handler.

O código a seguir implementa a mesma funcionalidade de antes, porém usando middlewares e o padrão de Sessão por Requisição.

##### Middleware

```go
type CustomHandler func(http.ResponseWriter, *http.Request) error

type contextKey int

const (
	txContextKey contextKey = iota
)

func transaction(next CustomHandler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		tx, err := db.Begin()
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			fmt.Printf("Open transaction failed: %s \n", err.Error())
			return
		}

		r = r.WithContext(context.WithValue(r.Context(), txContextKey, tx))

		defer func() {
			if r := recover(); r != nil {
				var err error
				switch r := r.(type) {
				case error:
					err = r
				default:
					err = fmt.Errorf("%v", r)
				}
				w.WriteHeader(http.StatusInternalServerError)
				fmt.Printf("Transaction is being rolled back: %s \n", err.Error())
				tx.Rollback()
				return
			}
		}()

		err = next(w, r)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			fmt.Printf("Transaction is being rolled back: %s \n", err.Error())
			tx.Rollback()
			return
		}

		err = tx.Commit()
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			fmt.Printf("Transaction commit failed: %s \n", err.Error())
		} else {
			fmt.Println("Transaction has been committed")
		}
	})
}
```

#### Handler

```go
func inder(w http.ResponseWriter, r *http.Request) error {
	tx := r.Context().Value(txContextKey).(*sql.Tx)

	var id int
	err := tx.QueryRow("SELECT id FROM pages WHERE url = $1", r.URL.Path).Scan(&id)
	if err != nil && err != sql.ErrNoRows {
		return err
	}

	if id == 0 {
		err := tx.QueryRow("INSERT INTO pages (url, visitors) VALUES ($1, 0) RETURNING id", r.URL.Path).Scan(&id)
		if err != nil {
			return err
		}
	}

	_, err = tx.Exec("UPDATE pages SET visitors = visitors + 1 WHERE id = $1", id)
	if err != nil {
		return err
	}

	_, err = tx.Exec("INSERT INTO page_visitors (page_id, ip, datetime) VALUES ($1, $2, $3)", id, r.RemoteAddr, time.Now())
	if err != nil {
		return err
	}

	w.Write([]byte(fmt.Sprintf("Thanks for visiting '%s'", r.URL.Path)))
	return nil
}
```

O código do `index` ficou muito menor e mais simples agora. Tudo o que é feito é extrair a transação ativa que está no contexto e utilizá-la. Se algo errado acontecer, a função retornar um objeto `error` para o middleware. O codigo do middleware pode parecer meio estranho, mas depois criado basta reutilizá-lo em qualquer outro handler da aplicação.

Este padrão também traz alguns benefícios, como por exemplo:

1. É mais fácil escrever testes unitários para o handler já que podemos injetar a transação através do contexto;
2. O handler não precisa tratar os errors, basta devolvê-lo à quem o invocou;
3. Todo a requisição HTTP funciona como uma única unidade de trabalho. Todas as operações com o banco de dados que foram feitas durante a requisição irão ser commitadas (ou rolled back) no mesmo momento, independente de que função executou o comando.
4. O código é mais fácil de entender e reutilizar já que estamos seguindo o [Princípio da Responsabilidade Singular](https://en.wikipedia.org/wiki/Single_responsibility_principle);

Por outro lado, você vai precisar fazer suas requisições HTTP completarem o mais rápido possível. Nunca é uma boa ideia ter várias transações com o banco de dados abertas por muito tempo.

### E agora, qual é o próximo passo?

O padrão pode ser implementado em qualquer projeto Go já que não depende de nenhum framework.

Se você estiver procurando por inspiração, veja o código do [Fider](https://github.com/getfider/fider), um projeto Open Source onde este padrão (e muitas outras coisas) foram implementadas.

Espero que tenham gostado. Um abraço.