;;; darkman.el --- Seamless integration with Darkman  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Taha Aziz Ben Ali

;; Author: Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; Maintainer: Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; Homepage: https://darkman.grtcdr.tn
;; Version: 1.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; Permission to use, copy, modify, and/or distribute this software for
;; any purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; darkman.el provides seamless integration between Darkman and Emacs
;; using the D-Bus protocol.

;;; Code:

(require 'dbus)

(defgroup darkman nil
  "Seamless integration with Darkman."
  :group 'convenience
  :prefix "darkman-")

(defcustom darkman-after-mode-changed-hook nil
  "Hook run after the mode is changed."
  :type 'hook
  :group 'darkman)

(defcustom darkman-themes '(:light modus-operandi :dark modus-vivendi)
  "A property list which maps modes to their corresponding themes.

The two properties, ‘:light’ and ‘:dark’, expect a valid theme
symbol. Themes are considered valid if listed in the
`custom-available-themes' variable or selectable via the
`customize-themes' interface.

Consider the following configuration:

    (setq darkman-themes \\='(:light modus-operandi :dark modus-vivendi))

That will set `modus-operandi' and `modus-vivendi' as the daytime
and nighttime themes, respectively."
  :type `(plist :key-type (choice (const :tag "Light theme" :light)
				  (const :tag "Dark theme" :dark))
		:value-type (choice ,@(mapcar (lambda (theme) (list 'const theme))
					      (custom-available-themes))))
  :package-version '(darkman . "0.1.0"))

(defcustom darkman-switch-themes-silently t
  "When non-nil, suppress the message indicating the new mode and theme."
  :type 'boolean
  :package-version '(darkman . "0.3.0"))

(defvar darkman--dbus-service "nl.whynothugo.darkman")
(defvar darkman--dbus-path "/nl/whynothugo/darkman")
(defvar darkman--dbus-interface darkman--dbus-service)
(defvar darkman--dbus-signal nil)
(defvar darkman--theme nil)

;;;###autoload
(defun darkman-current-mode (&optional message)
  "Return the mode of the service.
When MESSAGE is non-nil, print the current mode to the echo area instead."
  (interactive (list t))
  (let ((mode (dbus-get-property
	       :session
	       darkman--dbus-service
	       darkman--dbus-path
	       darkman--dbus-interface
	       "Mode")))
    (if message
	(message "Mode is currently set to %s." mode)
      mode)))

(defun darkman-set-mode (mode)
  "Set the mode of the service to MODE.
MODE can be ‘light’ or ‘dark’."
  (dbus-set-property :session
		     darkman--dbus-service
		     darkman--dbus-path
		     darkman--dbus-interface
		     "Mode" (if (member mode '(dark light))
				(symbol-name mode)
			      (darkman--invalid-mode-error mode))))

;;;###autoload
(defun darkman-toggle ()
  "Toggle the mode of the service."
  (interactive)
  (let ((mode (darkman-current-mode)))
    (cond ((string= mode "dark") (darkman-set-mode 'light))
	  ((string= mode "light") (darkman-set-mode 'dark)))))

;;;###autoload
(defun darkman-set-theme ()
  "Set the theme to match that of the current mode."
  (interactive)
  (let ((mode (darkman-current-mode)))
    (darkman--load-theme (darkman--lookup-theme mode))))

(defun darkman--invalid-mode-error (mode)
  "Signal an error about an invalid mode.  MODE is the name of the invalid mode."
  (error "‘%s’ is not a valid mode" mode))

(defun darkman--dbus-service-unavailable-error ()
  "Signal an error about ‘darkman--dbus-service’ being unavailable."
  (error "%s D-Bus service not available" darkman--dbus-service))

(defun darkman--lookup-theme (mode)
  "Return a theme from ‘darkman-themes’ corresponding to MODE."
  (cond ((string= mode "dark") (plist-get darkman-themes :dark))
	((string= mode "light") (plist-get darkman-themes :light))))

(defun darkman--lookup-mode (theme)
  "Return the mode corresponding to THEME."
  (cond ((string= theme (plist-get darkman-themes :dark)) "dark")
        ((string= theme (plist-get darkman-themes :light)) "light")))

(defun darkman--load-theme (theme)
  "Load and set ‘darkman--theme’ to THEME."
  (load-theme theme)
  (setq darkman--theme theme))

(defun darkman--signal-handler (mode)
  "Callback function for handling a change in mode.
MODE is the new mode."
  (when (not (string= mode (darkman--lookup-mode darkman--theme)))
    (let ((theme (darkman--lookup-theme mode)))
      (unless darkman-switch-themes-silently
        (message "Darkman switched to %s mode, switching to %s theme."
	         mode theme))
      (darkman--load-theme theme)
      (run-hooks 'darkman-after-mode-changed-hook))))

(defun darkman--check-dbus-service ()
  "Return non-nil if ‘darkman--dbus-service’ is available."
  (or (dbus-ping :session darkman--dbus-service 1000)
      (darkman--dbus-service-unavailable-error)))

;;;###autoload
(define-minor-mode darkman-mode
  "Minor mode providing integration with Darkman."
  :global t
  :init-value nil
  :require 'dbus
  :version "0.1.0"
  (if darkman-mode
      (unless (and darkman--dbus-signal
		   (not (darkman--check-dbus-service)))
	(setq darkman--dbus-signal
	      (dbus-register-signal
	       :session
	       darkman--dbus-service
	       darkman--dbus-path
	       darkman--dbus-interface
	       "ModeChanged"
	       #'darkman--signal-handler))
	(darkman-set-theme))
    (dbus-unregister-object darkman--dbus-signal)
    (setq darkman--dbus-signal nil)))

(provide 'darkman)
;;; darkman.el ends here
