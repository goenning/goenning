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
      <a href="https://goenning-my.sharepoint.com/:b:/g/personal/me_goenning_net/EXt9iDtzYOFMmrux6_bYzpkBgLCn45uJ7gPvQyKnYw0Y4g?e=Syh1ZN">Reducing bundle size of TypeScript applications with tslib</a>
      <span class="archive-post-date">October/2018 (Dublin - Ireland)</span>
    </li>
    <li>
      <a href="https://www.meetup.com/Dublin-TypeScript-Meetup/events/240961384/">How TypeScript can help you write better E2E tests</a>
      <span class="archive-post-date">August/2018 (Dublin - Ireland)</span>
    </li>
    <li>
      <a href="https://goenning-my.sharepoint.com/:p:/g/personal/me_goenning_net/ERJrhvOP_d9CsFlq6WP0ufUBQ2_YJh2RxDnStIFImMC6pw?e=nA3JpX">Introdução ao TypeScript + React</a>
      <span class="archive-post-date">Novembro/2016 (Blumenau/SC - Brazil)</span>
    </li>
    <li>
      <a href="https://goenning-my.sharepoint.com/:p:/g/personal/me_goenning_net/EaKDJIn9iTdOlM-Hc3zqEiQBMRYhk-ByametNatmaYeoxQ?e=G1NBBF">Aplicando Coding Dojo no Ensino Técnico</a>
      <span class="archive-post-date">Novembro/2010 (Joinville/SC - Brazil)</span>
    </li>
  </ul>
</div>