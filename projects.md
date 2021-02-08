---
layout: page
title: Projects
permalink: /projects
lang: en
ref: projects
---

Here's a list of open source projects I'm working on (or have worked in the past).

<ul class="projects-list">
{% for item in site.data.projects %}
    <li>
        <div>
            <h4>{{ item.name }} &middot; <a href="{{ item.link }}" target="_blank" rel="noopener noreferrer">{{ item.link }}</a></h4>
            <span>{{ item.desc_en }}</span>
            <a class="github" href="{{ item.github }}" target="_blank" rel="noopener noreferrer">{% include icons/github.svg %} {{ item.github }}</a>
        </div>
    </li>
{% endfor %}
</ul>
