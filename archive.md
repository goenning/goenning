---
layout: page
title: Blog Archive
permalink: /archive
lang: en
ref: archive
---

<div class="archives">
  <ul>
    {% assign posts=site.posts | where:"lang", page.lang %}
    {% for post in posts reverse %}
    	<li>
        <a href="{{post.url}}">{{post.title}}</a>
        <span class="archive-post-date">{{post.date | date_to_string }}</span>
      </li>
    {% endfor %}
  </ul>
</div>