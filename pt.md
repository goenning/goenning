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

      <div class="entry">
        {% if post.content contains '<!--more-->' %}
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