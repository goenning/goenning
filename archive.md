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

  <h2 id="talks">Talks</h2>
  <ul>
    <li>
      <a href="https://1drv.ms/b/s!AkX1H2lInF_rgcgeIxvFvMSkB7ZHrw?e=Z71608">Introduction to Angular Change Detection</a>
      <span class="archive-post-date">August/2019 (Dublin - Ireland)</span>
    </li>
    <li>
      <a href="https://1drv.ms/b/s!AkX1H2lInF_rgcgf1CWPqPzXVlrYyQ?e=ff5wPp">Reducing bundle size of TypeScript applications with tslib</a>
      <span class="archive-post-date">October/2018 (Dublin - Ireland)</span>
    </li>
    <li>
      <a href="https://www.meetup.com/Dublin-TypeScript-Meetup/events/240961384/">How TypeScript can help you write better E2E tests</a>
      <span class="archive-post-date">August/2018 (Dublin - Ireland)</span>
    </li>
    <li>
      <a href="https://1drv.ms/p/s!AkX1H2lInF_rgcggwgIFoj4SGzM-QQ?e=xccBMx">Introdução ao TypeScript + React</a>
      <span class="archive-post-date">Novembro/2016 (Blumenau/SC - Brazil)</span>
    </li>
    <li>
      <a href="https://1drv.ms/p/s!AkX1H2lInF_rgcghAi9w7jFpe6zXBA?e=Ep50QL">Aplicando Coding Dojo no Ensino Técnico</a>
      <span class="archive-post-date">Novembro/2010 (Joinville/SC - Brazil)</span>
    </li>
  </ul>

  {% assign tags = "" | split: "," %}
  {% for post in posts  %}
    {% assign tags = tags | concat: post.tags | sort %}
  {% endfor %}

  <h1>Tags</h1>  
  {% assign currTag = "" %}
  {% for tag in tags %}
    {% if currTag == "" %}
      {% assign currTag = tag %}
      {% assign tagCount = 1 %}
    {% else %}
      {% if currTag == tag %}
        {% assign tagCount = tagCount | plus: 1 %}
      {% else %}
        <span class="site-tag count-{{tagCount}}">
          <a href="/tag/en#{{ currTag | slugify }}">{{ currTag | replace:'-', ' ' }} ({{ tagCount }})</a>
        </span>
        {% assign currTag = tag %}
        {% assign tagCount = 1 %}
      {% endif %}
    {% endif %}
  {% endfor %}
</div>