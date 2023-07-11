



This is the manual for `darkman.el` version 1.1.0.

-   Homepage: <https://darkman.grtcdr.tn>
-   Repository: <https://git.sr.ht/~grtcdr/darkman.el>
-   Mailing list: <https://lists.sr.ht/~grtcdr/pub>
-   Issue tracker: <https://todo.sr.ht/~grtcdr/darkman.el>


# GNU Free Documentation License

Copyright (C) 2023 Taha Aziz Ben Ali.

> Permission is granted to copy, distribute and/or modify this document
> under the terms of the GNU Free Documentation License, Version 1.3 or
> any later version published by the Free Software Foundation; with no
> Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts. A
> copy of the license is included in the section entitled "GNU Free
> Documentation License".


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


# Usage

In your `init.el` file, define which theme is associated with which
mode. By default, `modus-themes` will be used, but we'll choose the
`tango` variants in this example.

    (setq darkman-themes '(:light tango :dark tango-dark))

You can also use `M-x customize-group darkman RET` to customize the
variable.

`darkman` can listen for any signals to change the theme without
requiring you to manually create any scripts. You can enable this
behavior by enabling the minor mode:

    (darkman-mode)

You should not call `load-theme` in your configuration as
`darkman-mode` when enabled will do that for you, even on startup.


## Other functions

`M-x darkman-toggle` will toggle the mode of the Darkman service from
within Emacs.

Additionally, you can use the `darkman-get` and `darkman-set`
functions to manually get and set the mode of the service
respectively.


# Tips


## Working alongside the built-in safety features

`load-theme` is part of the `custom` library and it comes with certain
[security features](emacs#Custom Themes) that you may regard as inconvenient, but they're
absolutely necessary to preserve the security of your system. We have
decided to work hand in hand with these pre-existing safety mechanisms
in lieu of implementing our own.

Problems may arise <sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup> if this package is loaded in a [server
setting](emacs#Emacs Server) and too early during the initialization phase, i.e. before
`custom-file`, you can prevent this from happening by activating the
mode **only** when the session has been fully initialized:

    (when (daemonp)
      (add-hook 'server-after-make-frame-hook #'darkman-mode)
      (advice-add 'darkman-mode
                  :after
                  (lambda ()
                    (remove-hook 'server-after-make-frame-hook
                                 #'darkman-mode))))


## Disabling existing themes

Emacs does not by default disable the current theme when another one
is loaded, and while this behavior might seem unintuitive, we can
always advise `darkman-set` (and by extension `darkman-toggle`), or
even `load-theme` if you prefer, to disable any existing themes first.

    (defadvice darkman-set (before no-theme-stacking activate)
      "Disable the previous theme before loading a new one."
      (mapc #'disable-theme custom-enabled-themes))


# Debugging


## Is D-Bus running?

Use your service manager to verify whether the service is running,
here's an example using `systemd`:

    systemctl status dbus

    dbus.service - D-Bus System Message Bus
       Loaded: loaded
       Active: active (running)


## Is Darkman running?

Use your service manager to verify whether the service is running,
here's an example using `systemd`:

    systemctl status --user darkman

    darkman.service - Framework for dark-mode and light-mode transitions.
       Loaded: loaded
       Active: active (running)


## Is Emacs built with D-Bus support?

Usually it is, unless you're building from source, in which case you
can verify whether or not Emacs was built with D-Bus support using
`C-h v system-configuration-features RET` which should list `DBUS`.


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> This is [known to happen](https://github.com/grtcdr/darkman.el/pull/7#issuecomment-1422666665) in Doom Emacs.
