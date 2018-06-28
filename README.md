TRAVERSAL
=========

This repository contains an implementation of a multiword expression (MWE)
identification system based on tree-structured conditional random fields
(CRFs). This system participated in the [PARSEME shared task][shared-task] on
automatic identification of verbal multiword expressions, edition 1.1, and
ranked 1st in the general cross-lingual ranking of the closed track systems.


Installation
============

First you will need to download and install the [Haskell Tool Stack][stack].
Then clone this repository into a local directory and use `stack` to install
the tool by running:

    stack install


Data format
===========

The tool works with the [.cupt][cupt] data format.


Configuration
=============

To train an MWE identification model, you will need to specify the configuration
of the system first. This is done using the [Dhall][dhall] programming language.
Configuration determines, in particular, the feature templates to be used for
MWE identification. The default configuration can be found in the
`config/config.dhall` file.


Training
========

Assuming that you have:

  * `train.cupt`: training file with MWE annotations in the [.cupt][cupt] format,
  * `dev.cupt`: development file with MWE annotation in the [.cupt][cupt] format (optional),
  * `config/config.dhall`: configuration file with feature templates,
  
and that you wish to identify MWEs of the category `VID` (each MWE in the
[.cupt][cupt] file should be marked with its category), then you can use the
following command to train the corresponding model and store it in the
`VID.model` file:

    traversal train -c config/config.dhall -t train.cupt -d dev.cupt --mwe VID -m VID.model


Tagging
=======

TODO



[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
[shared-task]: http://multiword.sourceforge.net/PHITE.php?sitesig=CONF&page=CONF_04_LAW-MWE-CxG_2018___lb__COLING__rb__&subpage=CONF_40_Shared_Task "PARSEME shared task - edition 1.1 (2018)"
[cupt]: http://multiword.sourceforge.net/PHITE.php?sitesig=CONF&page=CONF_04_LAW-MWE-CxG_2018___lb__COLING__rb__&subpage=CONF_45_Format_specification "The CUPT format"
[dhall]: https://github.com/dhall-lang/dhall-lang "Dhall"
