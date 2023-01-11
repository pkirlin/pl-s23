---
layout: home
title: Home
nav_exclude: false
nav_order: 0
seo:
  type: Course
  name: Just the Class
---

# {{ site.tagline }}
{: .mb-2 }
{{ site.description }}
{: .fs-6 .fw-300 }

{% if site.announcements %}
{{ site.announcements.last }}
[Announcements](announcements.md){: .btn .btn-outline .fs-3 }
{% endif %}

## Administrivia
- Instructor: Phillip Kirlin
- Office hours: TBA.  Also available by appointment and over Slack.
- [Canvas page](https://rhodes.instructure.com/courses/4688): Use for grades, online assignment submissions, and assignment solutions.
- [Syllabus](syllabus/syllabus-pl-s23.pdf) and [additional policies](syllabus/additional-policies.pdf).

## Resources
     

## Calendar
{% for module in site.modules %}
{{ module }}
{% endfor %}

