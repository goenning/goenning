---
layout: page
title: Projects
permalink: /projects
lang: en
ref: projects
---

Here's a list of open source projects I'm working on (or have worked in the past).

<div>
{% include aptakube.html lang="en" %}
</div>

<ul class="projects-list">
{% for item in site.data.projects %}
    <li>
        <div>
            <h4>{{ item.name }} &middot; <a href="{{ item.link }}" target="_blank" rel="noopener">{{ item.link }}</a></h4>
            <span>{{ item.desc_en }}</span>
            {% if item.github %}
                <a class="github" href="{{ item.github }}" target="_blank" rel="noopener">{% include icons/github.svg %} {{ item.github }}</a>
            {% endif %}
        </div>
    </li>
{% endfor %}
</ul>