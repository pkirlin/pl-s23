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
- Office hours: Mon 12-1, Tue 9-10:30, Wed 3-4, Thu 10-11:30.  Also available by appointment and over Slack.
- [Canvas page](https://rhodes.instructure.com/courses/4029): Use for grades, online assignment submissions, and assignment solutions.
- [Syllabus](syllabus/syllabus-142-f22.pdf) and [additional policies](syllabus/additional-policies.pdf).
- Tutoring hours: Sunday through Thursday evenings, 5-11pm, Briggs 001

## Resources
- Textbooks and tutorials: *Introduction to Java* by Liang (textbook), 
	*Introduction to Programming in Java* by Sedgewick and Wayne (textbook),
        [official Java tutorials](https://docs.oracle.com/javase/tutorial/), 
        [Introduction to Programming Using Java](http://math.hws.edu/javanotes/index.html) (free online textbook)
- Java in the browser: [Repl.it](http://repl.it/new/java), <a href="http://codehs.com">CodeHS</a>
- <a href="https://docs.oracle.com/en/java/javase/17/docs/api/">Official Java documentation</a>
     

## Calendar
{% for module in site.modules %}
{{ module }}
{% endfor %}

