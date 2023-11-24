

# Introduction

This package provides seamless integration between [Darkman](https://darkman.whynothugo.nl) and [Emacs](https://gnu.org/software/emacs)
using the [D-Bus](https://www.freedesktop.org/wiki/Software/dbus/) protocol.

Documentation is available in [HTML](https://darkman.grtcdr.tn/), [PDF](https://darkman.grtcdr.tn/darkman.pdf) and Info.


# Installation

This package is available from [MELPA](https://melpa.org) provided you've added that to the
list of package archives to fetch from, install it by evaluating the
following:

    (package-install 'darkman)

For a manual installation, begin by cloning the repository:

    git clone --branch 1.0.3 https://github.com/grtcdr/darkman.el darkman

Next, add the package to your [load path](https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html):

    (add-to-list 'load-path "darkman")

Finally, require the package like so:

    (require 'darkman)


# Support

If you wish to contribute a patch, inquire about something or share
your feedback, you are welcome to send an email to
[~grtcdr/pub@lists.sr.ht](mailto:~grtcdr/pub@lists.sr.ht). If you encounter issues of any kind, please
file them in the project's [ticket tracker](https://todo.sr.ht/~grtcdr/darkman.el).


# Acknowledgment

I'd like to thank the following people for helping to improve this package:

-   Agustín Cisneros
-   Aleksei Fedotov
-   Chris Rayner
-   Jonas Bernoulli
-   Nicolas Vollmer

> Want to contribute? Pick something from the [to-do list](https://darkman.grtcdr.tn/TODO.html).

