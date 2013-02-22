Purpose:

To provide a pleasant mode to browse and edit Haskell files, linking
into the following supported modules:

`haskell-font-lock', Graeme E Moss and Tommy Thorn
  Fontifies standard Haskell keywords, symbols, functions, etc.

`haskell-decl-scan', Graeme E Moss
  Scans top-level declarations, and places them in a menu.

`haskell-doc', Hans-Wolfgang Loidl
  Echoes types of functions or syntax of keywords when the cursor is idle.

`haskell-indentation', Kristof Bastiaensen
  Intelligent semi-automatic indentation, mark two.

`haskell-indent', Guy Lapalme
  Intelligent semi-automatic indentation.

`haskell-simple-indent', Graeme E Moss and Heribert Schuetz
  Simple indentation.

`inf-haskell'
  Interaction with an inferior Haskell process.
  It replaces the previous two modules:
    `haskell-hugs', Guy Lapalme
    `haskell-ghci', Chris Web


This mode supports full Haskell 1.4 including literate scripts.
In some versions of (X)Emacs it may only support Latin-1, not Unicode.

History:

This mode is based on an editing mode by Simon Marlow 11/1/92
and heavily modified by Graeme E Moss and Tommy Thorn 7/11/98.

If you have any problems or suggestions specific to a supported
module, consult that module for a list of known bugs, and an
author to contact via email.  For general problems or suggestions,
consult the list below, then email gem@cs.york.ac.uk and
thorn@irisa.fr quoting the version of the mode you are using, the
version of Emacs you are using, and a small example of the problem
or suggestion.

Version 1.5
  Added autoload for haskell-indentation

Version 1.43:
  Various tweaks to doc strings and customization support from
  Ville Skyttä <scop@xemacs.org>.

Version 1.42:
  Added autoload for GHCi inferior mode (thanks to Scott
  Williams for the bug report and fix).

Version 1.41:
  Improved packaging, and made a couple more variables
  interactively settable.

Version 1.4:
  Added GHCi mode from Chris Webb, and tidied up a little.

Version 1.3:
  The literate or non-literate style of a buffer is now indicated
  by just the variable haskell-literate: nil, `bird', or `tex'.
  For literate buffers with ambiguous style, the value of
  haskell-literate-default is used.

Version 1.2:
  Separated off font locking, declaration scanning and simple
  indentation, and made them separate modules.  Modules can be
  added easily now.  Support for modules haskell-doc,
  haskell-indent, and haskell-hugs.  Literate and non-literate
  modes integrated into one mode, and literate buffer indicated by
  value of haskell-literate(-bird-style).

Version 1.1:
  Added support for declaration scanning under XEmacs via
  func-menu.  Moved operators to level two fontification.

Version 1.0:
  Added a nice indention support from Heribert Schuetz
  <Heribert.Schuetz@informatik.uni-muenchen.de>:

    I have just hacked an Emacs Lisp function which you might prefer
    to `indent-relative' in haskell-mode.el.  See below.  It is not
    really Haskell-specific because it does not take into account
    keywords like `do', `of', and `let' (where the layout rule
    applies), but I already find it useful.

  Cleaned up the imenu support.  Added support for literate scripts.

Version 0.103 [HWL]:
  From Hans Wolfgang Loidl <hwloidl@dcs.gla.ac.uk>:

  I (HWL) added imenu support by copying the appropriate functions
  from hugs-mode.  A menu-bar item "Declarations" is now added in
  haskell mode.  The new code, however, needs some clean-up.

Version 0.102:

  Moved C-c C-c key binding to comment-region.  Leave M-g M-g to do
  the work.  comment-start-skip is changed to comply with comment-start.

Version 0.101:

  Altered indent-line-function to indent-relative.

Version 0.100:

  First official release.

Present Limitations/Future Work (contributions are most welcome!):

. Would like RET in Bird-style literate mode to add a ">" at the
  start of a line when previous line starts with ">".  Or would
  "> " be better?

. Support for GreenCard?
