This package provides seamless integration between [Darkman](https://darkman.whynothugo.nl) and [Emacs](https://gnu.org/software/emacs)
using the [D-Bus](https://www.freedesktop.org/wiki/Software/dbus/) protocol.

Documentation is available in a number of formats:

-   [HTML](https://grtcdr.tn/darkman.el/MANUAL.html)
-   [PDF](https://grtcdr.tn/darkman.el/MANUAL.pdf)
-   [Texinfo](MANUAL.texi)


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


# Contributors

> Want to contribute to the package? Pick something from the [to-do list](https://grtcdr.tn/darkman.el/TODO.html).

-   Agustín Cisneros
-   Aleksei Fedotov
-   Chris Rayner
-   Jonas Bernoulli
-   Nicolas Vollmer


# Support

If you wish to contribute a patch, inquire about something or share
your feedback, you are welcome to send an email to
[~grtcdr/pub@lists.sr.ht](mailto:~grtcdr/pub@lists.sr.ht). If you encounter issues of any kind, please
file them in the project's [ticket tracker](https://todo.sr.ht/~grtcdr/darkman.el).


# Citing

If your research involves this project in any way, you may cite it
like so:

    @misc{ab23darkman,
      author = {Aziz Ben Ali},
       title = {Seamless integration between Darkman and Emacs using the D-Bus protocol},
         url = {https://grtcdr.tn/darkman.el/},
        year = 2023
    }

