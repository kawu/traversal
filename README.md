TRAVERSAL
=========

This repository contains an implementation of a multiword expression (MWE)
identification system based on tree-structured conditional random fields. This
system participated in the [PARSEME shared task][shared-task] on automatic
identification of verbal multiword expressions, edition 1.1, and ranked 1st in
the general cross-lingual ranking of the closed track systems.

TRAVERSAL divides the task of MWE identification into two subsequent sub-tasks:

  * Dependency tree labeling (with two possible labels: `MWE` and `not-MWE`)
  * MWE segmentation (determining the boundaries of MWEs)
  
For the former task, the system encodes the possible labelings of dependency
trees as tree traversals so as to capture unary, binary, and ternary relations
between nodes, their parents, and their siblings. Then it strives to find the
globally optimal traversal for a given dependency tree based on the multiclass
logistic regression model.

For MWE segmentation, the system relies on a rather rudimentary solution -- by
default, all adjacent dependency nodes marked as MWEs of the same category are
assumed to form a single MWE occurrence.


Installation
============

First you will need to download and install the [Haskell Tool Stack][stack].
Then clone this repository into a local directory and use `stack` to install
the tool by running:

    stack install
    
The above command builds the `traversal` command-line tool and (on Linux) puts
it in the `~/.local/bin/` directory by default. Installation on Windows, even
though not tested, should be also possible.


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


MWE identification
==================

Once you train the model, you can perform MWE identification using the following
command:

    traversal tag -c config/config.dhall --mwe VID -m VID.model < test.cupt
    
where `test.cupt` is the input file (in the [.cupt][cupt] format). The command
outputs the MWE identification results to `stdout.` You must use the same
configuration (`-c config/config.dhall`) as during training.

MWE annotations already present on input (if any) will be copied on output. This
allows to annotate a single file based on several models corresponding to
different MWE categories.


MWE segmentation
----------------

By default, all adjacent dependency nodes marked as MWEs of the same category
are assumed to form a single MWE occurrence. Another segmentation heuristic
available in the system is to split each group of adjacent MWE-marked nodes into
two (or more) distinct MWEs, depending on how many nodes with a certain POS
value it contains.

For instance, to divide each MWE-marked group into two (or more) distinct MWEs
when it contains two (or more) verbs, use the following command:

    traversal tag -c config/config.dhall --mwe VID -m VID.model --split VERB < test.cupt
    
Both heuristics provided in the system are rather rudimentary and we plan to
replace them by a more advanced solution to MWE segmentation in the future.

Clearing
--------

If you want to remove MWE annotations from a given `test.cupt` file, use:

    traversal clear < test.cupt
    


[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
[shared-task]: http://multiword.sourceforge.net/PHITE.php?sitesig=CONF&page=CONF_04_LAW-MWE-CxG_2018___lb__COLING__rb__&subpage=CONF_40_Shared_Task "PARSEME shared task - edition 1.1 (2018)"
[cupt]: http://multiword.sourceforge.net/PHITE.php?sitesig=CONF&page=CONF_04_LAW-MWE-CxG_2018___lb__COLING__rb__&subpage=CONF_45_Format_specification "The CUPT format"
[dhall]: https://github.com/dhall-lang/dhall-lang "Dhall"
