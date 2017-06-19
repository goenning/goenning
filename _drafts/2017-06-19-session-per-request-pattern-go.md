---
layout: post
title: "Session per Request pattern in Go"
comments: true
lang: en
tags: [go, golang, web, database, transaction, pattern]
abstract: 
  TODO
---

Prior to start coding in Go, I've implemented multiple web applications in C# and Java. There is a particular pattern that is very common on these languages that it's easy to see any mentions about in the Go community.

The pattern is called **Session per Request** (aka: Transaction per Request) and is particularly very useful to decouple business objects from infrastructural details like database transaction management.

The idea behind this pattern is to avoid having to open and close transaction explicitly for each batch of database operation we need and move this into a proper component that does it automatically. In regards of being a web application, the transaction is open on the very beginning of the HTTP request and committed or rolled back before it ends. 

### Managing database transactions, the traditional way

The following code is an example of the traditional and most common way of handling a database transaction, not only in Go, but any language.

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

What it does is basically open a new transaction and run a couple of commands. If any error occurs, at any time, the transaction is rolled back and a 500 response is returned. If everything goes well, transaction is committed and a 200 response is returned with a simple message.

On this example we've basically mixed transaction management with a couple of business commands, and it's a bit bigger because of all the error handling statements.

Thsis is very common requirement for proper database consistency management. Now imagine, how messy this would become to spread this around all your codebase.

### Applying the pattern

Let's have a look on how we can improve this.

As I wrote before, the pattern consists of moving the transaction management code to another layer that can execute code **before and after** all the handlers. Go – and many other languages – can achieve this by using HTTP middlewares.

We'll create a middleware that opens a transaction before any handler is executed and it'll commit or rollback the transaction after handler finishes. By doing so, we can remove any transaction specific code from our handler.

The following code implements the same feature as code above, but using middlewares and Session per Request pattern.

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

The middleware creates a new context with recently created transaction and send it down to the pipeline. Once the execution comes back to the middleware, the transaction is either committed or rolled back.

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

The `index` handler code is much smaller and simple now. All it does is get an active transaction from the context and start using it. If something goes wrong, just return an `error` to the middleware. The middleware, of course, looks a bit scary at first, but it's something you write once and just reuse it.

This pattern also comes with a couple of benefits, such as:

1. Code is easier to maintain and reuse as it's following the [Single Responsibility Principle](https://en.wikipedia.org/wiki/Single_responsibility_principle);
2. It's easy to write unit tests for the handler as you can inject a transaction to the handler through the context;
3. The handler does not need to handle errors that it does not know how to, just send it back to the pipeline.
4. The whole HTTP request works as a single unit of work. Either everything is committed or everything fails, no matter which components have issued database commands.

On the other hand, you might want to keep your HTTP requests as fast as possible. It's never good to have lots of transactions hanging for a long time 😃.

### Where to go from here?

This pattern can be easily implemented in bare Go code or any other web framework. 

If you're looking for some inspiration, you can check my current OSS project called [Fider](https://github.com/getfider/fider) where I'm using echo framework, this pattern and plenty more things.

*I'd like hear back from you. What do you think? Is this useful? Did I miss something?*

Cheers!