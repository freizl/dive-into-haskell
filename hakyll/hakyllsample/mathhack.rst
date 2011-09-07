---
title: reST
author: haisgwu
date: 04/15/2011
---

reference form mathhack-rst_

A Math Hack for reST
====================
Using the 'raw' Directive and Role
----------------------------------

:Author: Alan G. Isaac
:Date: 8 Nov 2004

This is a brief discussion of a 'stop-gap' method of
including math in reST documents.  It is a 'stop-gap' in the
sense that only the LaTex and the HTML writers are
targetted.

The basic procedure can be summarized as follows.  Create a
reST document containing Itex_.  To create the corresponding
LaTeX document, process your reST file with rst2latex.py.
To create the corresponding XML document, process your reST
file with rst2html.py, and then process the result with
itex2mml.  Details follow.

.. warning::
  I am just a user, not a developer, and I am new to reST.
  These notes just reflect my experience and should not be
  viewed as reflecting the views of the reST developers (or
  anyone else, for that matter).

Display Math
------------

We will handle display math using the ``raw`` directive.
Here is an example of use of the directive::

  .. raw:: latex html

    \[  f(x)=\frac{e^{X\beta}}{1-e^{X\beta}}  \]

(Note the specification of the target writers.)

.. caution::
  In the XML version of this document, you will see display
  math in place of the raw directive that produces it.
  See the "Limitations" section below.

Here is the math display resulting from the Itex:

.. raw:: latex html

  \[  f(x)=\frac{e^{X\beta}}{1-e^{X\beta}}  \]


Inline Math
-----------

We will handle inline math using the ``raw`` role.  This
role cannot be used directly.  Instead we create a new role
based on ``raw``.

Here is an example of registering such a role, which I have
arbitrarily called ``raw-math``::

  .. role:: raw-math(raw)
     :format: html latex

The docutils writers generate no text from the role registration, of course.

.. role:: raw-math(raw)
   :format: html latex

Note once again the specification of specific writers.
Once we have registered our custom role, we can use it.  
For example, ``:raw-math:`$f(x)=x^2$```
produces the inline math :raw-math:`$f(x)=x^2$`. 

.. caution::
  The preceding paragraph illustrates the use of our
  ``raw-math`` role, but one piece of unwanted translation
  takes place in the XML version of this document.
  See the "Limitations" section below.



Document Generation
-------------------

LaTeX Generation
^^^^^^^^^^^^^^^^

Generation of LaTeX output is now a single step::

  rst2latex.py --use-latex-citations c:/temp.rst c:/temp.tex

Although it is irrelevant to the current document, I have
included the citation option because I believe it is always
wanted whenever citations are produced.  (I.e., the default
is set incorrectly, in my view.)


XML Generation
^^^^^^^^^^^^^^

Generation of XML output (as XHTML+MathML) is three-step process.  First::

  rst2html.py c:/temp.rst c:/temp.htm

will produce an HTML document with embedded Itex.
The Itex will display raw,
since it will not be interpreted by your browser.
To produce a proper math display in a browser,
we need to turn this into an XHTML+MathML document.

First you may need to ``tidy`` up your HTML document using the version of ``tidy`` supplied with itex2mml_::

  tidy -4mz < c:/temp.htm > c:/temp.xhtml

It may be possible to skip the ``tidy`` step:
I have tried and have seen no problems, but you are on your own here.
Finally, we need to transform it to XHTML+MathML as follows::

  itex2mml < temp.xhtml > temp.xml

You now have an XML document (XHTML+MathML) that you can
view in `MathML-compliant browsers`_.  At this time,
compliance is good in Amaya and in the Mozilla based
browsers (including Netscape and Firefox), and compliance is
improving in Internet Explorer (which currently requires
the MathPlayer plugin).


Limitations
-----------

- You can pass any valid LaTeX raw with the LaTeX writer, but
  if you also want to produce  XML documents (as
  XHTML+MathML), you need to restrict yourself to the
  intersection of Itex and LaTeX.  This is primarily important
  for arrays, where Itex adopts the `WebTeX array syntax`_.

- Similarly, you can pass any valid Itex raw with the HTML
  writer, but if you also want to produce LaTeX documents, you
  need to restrict yourself to the intersection of Itex and
  LaTeX.

- The two cautionary notes above refer to a problem in
  passing Itex math delimiters *uninterpreted* to an XML document.
  This affects illustrations of the underlying math code,
  but does not affect one's ability to display math.
  The problem is that itex2mml will always translate
  as math anything found inside the Itex math delimiters,
  even if these are inside PRE or TT elements.

  For example, if you try to show the underlying code for a
  math display by putting it inside a literal block, the HTML
  writer correctly passes the literal text inside a PRE
  element to the HTML document. However itex2mml does not
  currently recognize the PRE element as a block where Itex
  translation should be suppressed.  As a result, the Itex
  expression in the PRE element is extracted by itex2mml and
  replaced in the XML document with MathML, which the browser
  then displays. 

  A similar problem arises with inline math.  To illustrate
  the Itex code for the expression :raw-math:`$f(x)=x^2$`, we
  should write ``f(x)=x^2`` surrounded by dollar signs, but
  the dollar signs will cause a itex2mml to translate the
  surrounded expression.  The HTML writer passes the literal
  Itex inside a TT element to the HTML document, but itex2mml
  does not currently recognize TT elements as areas where Itex
  translation should be suppressed.  As a result, the Itex
  expression in the TT element is extracted by itex2mml and
  replaced in the XML document with MathML, which the browser
  then displays.

  This problem has been reported to the itex2mml author.  Of
  course the problem does not affect your ability to display
  mathematics, but only your ability to display the underlying
  Itex code *along with* the Itex delimiters.

Odds and Ends
-------------

Both rst2latex.py and rst2html.py are provided with
docutils_, but the itex2mml package_ must be downloaded
separated.  Currently the itex2mml package_ provided by Paul
Gartside contains ANSI C source and binaries for i686 linux.
Compiling the sources is fairly straightforward.  For
Windows, Jeff Tackett has provided compilation instructions_
along with a mingw compiled `itex2mml binary`_ and
associated `tidy binary`_.  I expect these to be included in
Gartside's package soon.



.. _Itex: http://pear.math.pitt.edu/mathzilla/itex2mmlItex.html
.. _docutils: http://docutils.sourceforge.net/
.. _itex2mml: http://pear.math.pitt.edu/mathzilla/itex2mmlItex.html
.. _package: http://pear.math.pitt.edu/mathzilla/itex2mml.tar.gz
.. _instructions: http://www.american.edu/econ/itex2mml/itex2mml_Instructions.txt
.. _itex2mml binary: http://www.american.edu/econ/itex2mml/itex2mml.exe
.. _tidy binary: http://www.american.edu/econ/itex2mml/tidy.exe
.. _WebTeX array syntax: http://stuff.mit.edu/afs/athena/software/webeq/currenthome/docs/webtex/wtxsec7.html#ARRAY
.. _MathML-compliant browsers: http://www.w3.org/Math/W3CDocs/mathmlrequ.html
.. _mathhack-rst: http://www1.american.edu/econ/itex2mml/mathhack.rst
