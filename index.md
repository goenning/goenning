---
layout: default
lang: en
---

<div class="posts">
  {% assign posts=site.posts | where:"lang", page.lang %}
  {% for post in posts limit:10 %}
    <article class="post">

      <h1><a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a></h1>

      {% if post.cover %}
        <img class="cover" src="{{ post.cover }}" />
      {% endif %}

      <div class="entry">
        {% if post.description %}
          <p>{{ post.description }}</p>
        {% elsif post.content contains '<!--more-->' %}
          {{ post.content | split:'<!--more-->' | first }}
        {% else %}
          {{ post.excerpt }}
        {% endif %}
      </div>

      <a href="{{ site.baseurl }}{{ post.url }}" class="read-more">Read More</a>
    </article>
  {% endfor %}
</div>

<p>See the <a href="{{ site.baseurl }}/archive">archive</a> for more posts.</p>