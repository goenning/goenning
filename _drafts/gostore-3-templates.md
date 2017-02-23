

### Leveling up with html/template

Now let's get back to what we're trying to achieve with our project. Our home page will be responsible for rendering multiple products and its images, so what I've done is create a package named `models` with structs for `Product`, `Image` and `IndexViewModel`.

```go
package models

// Image is an array of bytes of any image
type Image []byte

// Product model of something we have to sell
type Product struct {
	Title       string
	Price       float32
	Description string
	Images      []Image
}

// IndexViewModel is used by our index page
type IndexViewModel struct {
	Title    string
	Products []Product
}
```

Index handler has been moved to its own package named `handlers` and also changed to return some fake products for now.

```go
package handlers

import (
	"net/http"

	"github.com/goenning/gostore/models"
)

//Index is our main page handler
func Index(w http.ResponseWriter, r *http.Request) {
	products := []models.Product{
		{
			Title: "iMac 27\" 5K",
			Price: 1600,
		},
		{
			Title: "Giant Bike in mint conditions",
			Price: 1299.99,
		},
	}
	data := &models.IndexViewModel{
		Title:    "Go Store :)",
		Products: products,
	}

	w.WriteHeader(http.StatusOK)
	render(w, "index.html", data)
}
```

The only code that changed on `main.go` is `r.HandleFunc("/", index)`, which now has to reference the package where the handler can be found `r.HandleFunc("/", handlers.Index)`.

Our view has also changed to loop over all products and print its Title and Price.

{% raw %}
```html
<!doctype html>
<html>
    <head>
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width,initial-scale=1">
        <title>{{.Title}}</title>
    </head>
    <body>
      Welcome to your own <strong>{{.Title}}</strong>

    <ul>
        {{range .Products}}
            <li>{{.Title}}: {{.Price | currency}}</li>
        {{end}}
    </ul>
    </body>
</html>
```
{% endraw %}

If you look at this template carefully you'll notice the use of `| currency`. This is an user defined function that can be used on views to enchance/modify the visualization of some date. In this case, `Currency` is formatting our price to a human friendly string.

This is done by using a custom `render` function that is described below.

```go
package handlers

import (
	"html/template"
	"net/http"

	"github.com/leekchan/accounting"
)

var tpl *template.Template
var err error

// CurrencyExpander formats currency based on current Accounting settings
func CurrencyExpander(args ...interface{}) string {
	ac := accounting.Accounting{Symbol: "$", Precision: 2}
	value, _ := args[0].(float32)
	return ac.FormatMoney(value)
}

func loadTemplates() {
	tpl, err = template.New("").Funcs(template.FuncMap{
		"currency": CurrencyExpander,
	}).ParseGlob("views/*.html")

	if err != nil {
		panic(err)
	}
}

func render(w http.ResponseWriter, name string, data interface{}) {
	if tpl == nil {
		loadTemplates()
	}
	tpl.ExecuteTemplate(w, name, data)
}
```

When we load the templates, it's possible to register some functions and give it a name to be used in the templates. In this example we're also using a third-party package named `github.com/leekchan/accounting`, so don't forget to `glide get github.com/leekchan/accounting` it.