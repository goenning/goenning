---
layout: default
lang: pt
permalink: /pt
---


<div class="posts">
  {% assign posts=site.posts | where:"lang", page.lang %}
  {% for post in posts limit:10 %}
    <article class="post">

      <h1><a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a></h1>

      <small>
        {% include post_date.html lang=post.lang date=post.date %}
        {% include post_read_time.html lang=post.lang content=post.content %}
      </small>

      <div class="entry">
        {% if post.abstract %}
          <p>{{ post.abstract }}</p>
        {% elsif post.content contains '<!--more-->' %}
          {{ post.content | split:'<!--more-->' | first }}
        {% else %}
          {{ post.excerpt }}
        {% endif %}
      </div>

      <a href="{{ site.baseurl }}{{ post.url }}" class="read-more">CONTINUE LENDO</a>
    </article>
  {% endfor %}
</div>

<p>Veja o <a href="{{ site.baseurl }}/arquivo">arquivo</a> para mais posts.</p>