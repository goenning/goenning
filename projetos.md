---
layout: page
title: Projetos
permalink: /projetos
lang: pt
ref: projects
---

Uma lista de projetos open source que eu mantenho (ou mantive no passado).

<ul class="projects-list">
{% for item in site.data.projects %}
    <li>
        <div>
            <h4>{{ item.name }} &middot; <a href="{{ item.link }}">{{ item.link }}</a></h4>
            <span>{{ item.desc_pt }}</span>
            <a class="github" href="{{ item.github }}">{% include icons/github.svg %} {{ item.github }}</a>
        </div>
    </li>
{% endfor %}
<ul>