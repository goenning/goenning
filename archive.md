---
layout: page
title: Blog Archive
---

<div class="archives">
  <ul>
    {% for post in site.posts reverse %}
    	<li>
        <a href="{{post.url}}">{{post.title}}</a>
        <span class="archive-post-date">{{post.date | date_to_string }}</span>
      </li>
    {% endfor %}
  </ul>
</div>