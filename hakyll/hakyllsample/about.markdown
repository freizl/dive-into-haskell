---
title: About
---

[My douban](http://www.douban.com/people/freizl/)

# TODOs
#. (pageiation)[https://groups.google.com/group/hakyll/browse_thread/thread/15996c0ca132a1fc]
#. (slidy/S5 slides generator)[https://groups.google.com/group/hakyll/browse_thread/thread/994b1e0f550ccc90/3874f67ccf04264b#)
#. clean up untracked git object
     - why git-clean and git-gc failed to clean that big file
#. Chinese tag
#. Tag list with times

~~~~~~~~~
(requireA "tags" (setFieldA "taglist"
      (renderTagList tagIdentifier :: Compiler (Tags String) String))) 
~~~~~~~~~
