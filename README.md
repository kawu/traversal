TRAVERSAL
=========

This repository contains an implementation of a multiword expression (MWE)
identification system based on tree-structured conditional random fields
(CRFs). This system participated in the [shared task][shared-task] on automatic
identification of verbal multiword expressions, edition 1.1, and ranked 1st in
the general cross-lingual ranking of the closed track systems.


Installation
============

First you will need to download and install the [Haskell Tool Stack][stack].
Then clone this repository into a local directory and use `stack` to install
the tool by running:

    stack install


Data format
===========

The tool works with the [.cupt][cupt] data format.


Training
========

TODO


Tagging
=======

TODO



[shared-task] http://multiword.sourceforge.net/PHITE.php?sitesig=CONF&page=CONF_04_LAW-MWE-CxG_2018___lb__COLING__rb__&subpage=CONF_40_Shared_Task "PARSEME shared task - edition 1.1 (2018)"
[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
[cupt]: http://multiword.sourceforge.net/PHITE.php?sitesig=CONF&page=CONF_04_LAW-MWE-CxG_2018___lb__COLING__rb__&subpage=CONF_45_Format_specification "The CUPT format"
