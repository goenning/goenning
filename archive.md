---
layout: page
title: Blog Archives
---

<div class="archives">
  <ul>
    {% for post in site.posts reverse %}
    	<li><a href="{{post.url}}">{{post.title}}</a></li>
    {% endfor %}
  </ul>
</div>