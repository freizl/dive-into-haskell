---
title: Pandoc Syntax
author: haisgwu
date: 04/15/2011
---

[More markdown](http://daringfireball.net/projects/markdown/)

# Miscs

- I **like** several of their *flavors* of __ice cream__:
- 22, for _example_, and #5.
- This ~~is deleted text.~~
- H~2~O is a liquid.  2^10^ is 1024.
- autolink
    + <http://google.com>
    + This is an [inline link](http://www.yahoo.com)

# Images

Lambda  ![Lambda](/images/lambda.png)\ expression

![Haskell Wiki Logo](/images/haskellwiki_logo.png "Tooltip")

# Footnotes

Here is a footnote reference,[^1] and another.[^longnote]

[^1]: Here is the footnote.

[^longnote]: Here's one with multiple blocks.

    Subsequent paragraphs are indented to show that they 
belong to the previous footnote.

        { some.code }

    The whole paragraph can be indented, or just the first
    line.  In this way, multi-paragraph footnotes work like
    multi-paragraph list items.

This paragraph won't be part of the note, because it
isn't indented.

# Code

~~~~~~~{.haskell .numberLines}
qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++
               qsort (filter (>= x) xs) 
~~~~~~~

Term 1

:   Definition 1

Term 2 with *inline markup*

:   Definition 2

        { some code, part of Definition 2 }

    Third paragraph of definition 2.


# List

* fruits
    + apples
        - macintosh
        - red delicious
    + pears
    + peaches
* vegetables
    + brocolli
    + chard

+ A lazy, lazy, list
item.

+ Another one; this looks
bad but is legal.

    Second paragraph of second
list item.

 9)  Ninth
10)  Tenth
11)  Eleventh
       i. subone
      ii. subtwo
     iii. subthree

Note that Pandoc pays attention only to the starting marker in a list. So, the following yields a list numbered sequentially starting from 2:

(2) Two
(5) Three
1.  Four
*   Five

If default list markers are desired, use #.:

#.  one
#.  two
#.  three

<!-- end of list -->

+   First
+   Second:
    -   Fee
    -   Fie
    -   Foe

+   Third
<!-- end of list -->


# Table 

-------------------------------------------------------------
 Centered   Default           Right Left
  Header    Aligned         Aligned Aligned
----------- ------- --------------- -------------------------
   First    row                12.0 Example of a row that
                                    spans multiple lines.

  Second    row                 5.0 Here's another one. Note
                                    the blank line between
                                    rows.
-------------------------------------------------------------

Table: Here's the caption. It, too, may span
multiple lines.


  Right     Left     Center     Default
-------     ------ ----------   -------
     12     12        12            12
    123     123       123          123
      1     1          1             1


: Sample grid table.

+---------------+---------------+--------------------+
| Fruit         | Price         | Advantages         |
+===============+===============+====================+
| Bananas       | $1.34         | - built-in wrapper |
|               |               | - bright color     |
+---------------+---------------+--------------------+
| Oranges       | $2.10         | - cures scurvy     |
|               |               | - tasty            |
+---------------+---------------+--------------------+

