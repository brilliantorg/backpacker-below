# Camperdown Markup Language

Camperdown is a markup language for non-hypertext branching storytelling.
It tries to steal good ideas about branching-paths storytelling from the
[inkle](https://www.inklestudios.com/ink/) language, good ideas about markup
languages from [Idyll](https://idyll-lang.org/), and some syntax from
[elm](https://elm-lang.org/).

## Basic markup

Camperdown can be configured in different ways, but by default
it supports has _italic_, **bold**, and _**bold italic**_ text.

-   Lists are also
-   a thing you can do

    and list elements can contain multiple paragraphs.

> Blockquotes exist too.
>
>  — _The tooth fairy_

```
Camperdown can be configured in different ways, but by default
it supports has _italic_, **bold**, and _**bold italic**_ text.

! list >>
  : Lists are also
  : a thing you can do

    and list elements can contain multiple paragraphs.

! quote >>
    Blockquotes exist too.
 
    --- _The tooth fairy_
```

Camperdown's syntax is a little bit more persnickety and intrusive than 
[markdown](https://daringfireball.net/projects/markdown/basics).
The goal is to stay easy-to-learn and easy-to-read, but push the balance
a little further towards it being hard to accidentally write things that
get recognized as markup, and a _lot_ farther torwards being configurable
to different purposes.

_Line commands_ like `!list` and `!quote` organize text into indented sections.
This is meant to be configurable: different uses of Camperdown can have different
sets of commands. (A lot more than just commands are configurable, including the
annotations for bold and italics, and replacements like making `---` into an M-dash, 
but it's easier to introduce Camperdown by presenting some reasoanble defaults.)

A line command must have, as the first non-whitespace character,
a _marker_. The examples above show `!`, which is the _command marker_, and 
`:`, which is the _item marker_.