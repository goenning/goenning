---
layout: default
lang: pt
permalink: /tags/pt
title: Tags
ref: tags
---

<div class="posts">
  {% assign posts=site.posts | where:"lang", page.lang %}
  {% for post in posts %}
    {% assign ttags = post.tags | join:'|' | append:'|' %}
    {% assign rawtags = rawtags | append:ttags %}
  {% endfor %}
  {% assign rawtags = rawtags | split:'|' | sort %}
  {% assign tags = "" %}
  {% for tag in rawtags %}
    {% if tag != "" %}
      {% if tags == "" %}
        {% assign tags = tag | split:'|' %}
      {% endif %}
      {% unless tags contains tag %}
        {% assign tags = tags | join:'|' | append:'|' | append:tag | split:'|' %}
      {% endunless %}
    {% endif %}
  {% endfor %}
  {% for tag in tags %}
    <h2 id="{{ tag | slugify }}">{{ tag }}</h2>
    <ul class="posts">
    {% for post in posts %}
      {% if post.tags contains tag %}
      <li>
        <a href="{{post.url}}">{{post.title}}</a>
        <span class="archive-post-date">{% include post_date.html lang=post.lang date=post.date %}</span>
      </li>
      {% endif %}
    {% endfor %}
    </ul>
  {% endfor %}
</div>