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
- Office hours: Mon & Tue 3:30-5, Wed 10-11, Thu 11-12.  Also available by appointment and over Slack.
- [Canvas page](https://rhodes.instructure.com/courses/4688): Use for grades, online assignment submissions, and assignment solutions.
- [Syllabus](syllabus/syllabus-pl-s23.pdf) and [additional policies](syllabus/additional-policies.pdf).

## Resources
- [Downloading, installing, and using DrRacket (the Racket interpreter and IDE)](using-racket)
- Other Racket things: [main website](http://racket-lang.org), [documentation](https://docs.racket-lang.org/guide/index.html)
- Optional textbook: [Structure and Interpretation of Computer 
Programs](https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book.html)
- Online Scheme interpreters (very similar to Racket):
  - [Repl.it](https://replit.com/new/scheme), [One that can draw box-and-pointer diagrams](http://xuanji.appspot.com/js-scheme-stk/index.html)  (Try typing 
`'(1 2 3)` at the prompt.)

## Calendar
{% for module in site.modules %}
{{ module }}
{% endfor %}

