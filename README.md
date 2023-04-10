
# Table of Contents

1.  [Installation](#orgccc049a)
2.  [Contributors](#org4a424dc)
3.  [Citing](#orga4ec763)

This package [which has recently migrated to SourceHut](https://git.sr.ht/~grtcdr/darkman.el) provides seamless
integration between [Darkman](https://darkman.whynothugo.nl) and [Emacs](https://gnu.org/software/emacs) using the [D-Bus](https://www.freedesktop.org/wiki/Software/dbus/) protocol.

Documentation is available in a number of formats:

-   [HTML](https://grtcdr.tn/darkman.el/darkman.html)
-   [PDF](https://grtcdr.tn/darkman.el/darkman.pdf)
-   Texinfo


<a id="orgccc049a"></a>

# Installation

This package is available from [MELPA](https://melpa.org) provided you've added that to the
list of [package archives](emacs#Package Installation) to fetch from, install it by evaluating:

    (package-install 'darkman)

For a manual installation, begin by cloning the repository:

    git clone --branch 1.0.2 https://github.com/grtcdr/darkman.el darkman

Next, add the package to your [load path](https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html):

    (add-to-list 'load-path "darkman")

Finally, require the package like so:

    (require 'darkman)


<a id="org4a424dc"></a>

# Contributors

-   Agustín Cisneros
-   Aleksei Fedotov
-   Chris Rayner
-   Jonas Bernoulli
-   Nicolas Vollmer

> Want to contribute to the package? Pick something from the [to-do list](https://grtcdr.tn/darkman.el/TODO.html).


<a id="orga4ec763"></a>

# Citing

If your research involves this project in any way, you may cite it
like so:

    @misc{ab23darkman,
      author = {Aziz Ben Ali},
       title = {Seamless integration between Darkman and Emacs using the D-Bus protocol},
         url = {https://grtcdr.tn/darkman.el/},
        year = 2023
    }

