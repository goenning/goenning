---
layout: page
title: Arquivo do Blog
permalink: /arquivo
lang: pt
ref: archive
---

<div class="archives">
  <ul>
    {% assign posts=site.posts | where:"lang", page.lang %}
    {% for post in posts  %}
      {% capture this_year %}{{ post.date | date: "%Y" }}{% endcapture %}

      {% if forloop.first %}
      <h2 id="{{ this_year }}-ref">{{this_year}}</h2>
      <ul class="posts">
      {% else %}
          {% if this_year != last_year %}
          </ul>
          <h2 id="{{ this_year }}-ref">{{this_year}}</h2>
          <ul class="posts">
          {% endif %}
      {% endif %}

        <li>
          <a href="{{post.url}}">{{post.title}}</a>
          <span class="archive-post-date">{% include post_date.html lang=post.lang date=post.date %}</span>
        </li>

      {% if forloop.last %}
        </ul>
      {% endif %}

      {% capture last_year %}{{ this_year }}{% endcapture %}
    {% endfor %}
  </ul>

  <h2>Talks</h2>
  <ul>
    <li>
      <a href="https://drive.google.com/file/d/0B65Y6FNYSEywTXZJQ1NITXBJeE0/view">Introdução ao TypeScript + React</a>
      <span class="archive-post-date">Novembro/2016 (Blumenau/SC)</span>
    </li>
  </ul>
</div>