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
            <h4>{{ item.name }} &middot; <a href="{{ item.link }}" target="_blank" rel="noopener">{{ item.link }}</a></h4>
            <span>{{ item.desc_pt }}</span>
            {% if item.github %}
                <a class="github" href="{{ item.github }}" target="_blank" rel="noopener">{% include icons/github.svg %} {{ item.github }}</a>
            {% endif %}
        </div>
    </li>
{% endfor %}
</ul>