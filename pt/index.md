---
layout: default
lang: pt
---

<div class="posts">
  {% assign posts=site.posts | where:"lang", page.lang %}
  {% for post in posts limit:10 %}
    <article class="post">

      <h1><a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a></h1>

      <div class="entry">
        {{ post.excerpt }}
      </div>

      <a href="{{ site.baseurl }}{{ post.url }}" class="read-more">Read More</a>
    </article>
  {% endfor %}
</div>

<p>Veja o <a href="{{ site.baseurl }}/pt/arquivo">arquivo</a> para mais posts.</p>