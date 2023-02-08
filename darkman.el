;;; darkman.el --- Seamless integration between Darkman and Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://grtcdr.tn/darkman.el/
;; Version: 0.4.0
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

(defgroup darkman ()
  "Seamless integration between Darkman and Emacs."
  :group 'convenience)

(defcustom darkman-themes
  '(:light modus-operandi :dark modus-vivendi)
  "A property list which maps modes to their corresponding themes.

The two properties, ‘:light’ and ‘:dark’, expect as a value a
symbol representing the name of the theme."
  :type `(plist :key-type (choice (const :tag "Light theme" :light)
                                  (const :tag "Dark theme" :dark))
          :value-type (choice ,@(mapcar (lambda (theme) (list 'const theme)) (custom-available-themes))))
  :package-version '(darkman . "0.1.0"))

(defcustom darkman-switch-themes-silently t
  "Switch themes silently, as opposed to printing the switch action,
when the mode is changed."
  :type 'boolean
  :package-version '(darkman . "0.3.0"))

(defvar darkman--dbus-service "nl.whynothugo.darkman")
(defvar darkman--dbus-path "/nl/whynothugo/darkman")
(defvar darkman--dbus-interface darkman--dbus-service)
(defvar darkman--dbus-signal nil)

;;;###autoload
(defun darkman-get ()
  "Get the mode of the Darkman service."
  (interactive)
  (let ((mode (dbus-get-property :session
				 darkman--dbus-service
				 darkman--dbus-path
				 darkman--dbus-interface
				 "Mode")))
    (when (called-interactively-p 'interactive)
      (message (format "Mode is currently set to %s." mode)))
    mode))

(defun darkman-set (mode)
  "Set the mode of the Darkman service to MODE which can either be
‘light’ or ‘dark’."
  (dbus-set-property :session
		     darkman--dbus-service
		     darkman--dbus-path
		     darkman--dbus-interface
		     "Mode" (cond ((member mode '(dark light))
				   (symbol-name mode))
				  (t (darkman--invalid-mode-error mode)))))

;;;###autoload
(defun darkman-toggle ()
  "Toggle the mode of the Darkman service."
  (interactive)
  (let ((mode (darkman-get)))
    (cond ((string= mode "dark") (darkman-set 'light))
	  ((string= mode "light") (darkman-set 'dark)))))

(defun darkman--invalid-mode-error (mode)
  "Signal an error about an invalid mode.  MODE is the name of the invalid mode."
  (error (format "‘%s’ is not a valid mode." mode)))

(defun darkman--dbus-service-unavailable-error ()
  "Signal an error about the service being unavailable."
  (error (format "%s D-Bus service not available."
		 darkman--dbus-service)))

(defun darkman--lookup-theme (mode)
  "Return a theme from ‘darkman-themes’ corresponding to MODE (a string)."
  (cond ((string= mode "dark") (plist-get darkman-themes :dark))
	((string= mode "light") (plist-get darkman-themes :light))
	(t (darkman--invalid-mode-error mode))))

(defun darkman-get-theme ()
  "Get a theme from the ‘darkman-themes’ which corresponds to the current mode."
  (let ((mode (darkman-get)))
    (darkman--lookup-theme mode)))

(defun darkman--mode-changed-signal-handler (new-mode)
  "Signal handler for the ModeChanged signal.  NEW-MODE is the new mode."
  (let ((new-theme (darkman--lookup-theme new-mode)))
    (unless darkman-switch-themes-silently
      (message (format "Darkman switched to %s mode, switching to %s theme."
		       new-mode new-theme)))
    (load-theme new-theme)))

(defun darkman--check-dbus-service ()
  "Return non-nil if the Darkman service is available."
  (or (dbus-ping :session darkman--dbus-service 100)
      (darkman--dbus-service-unavailable-error)))

;;;###autoload
(define-minor-mode darkman-mode
  "Minor mode providing integration with Darkman."
  :global t
  :init-value nil
  :require 'dbus
  :version "0.1.0"
  (if darkman-mode
      (progn
	(and (darkman--check-dbus-service)
	     (setq darkman--dbus-signal
		   (dbus-register-signal
		    :session
		    darkman--dbus-service
		    darkman--dbus-path
		    darkman--dbus-interface
		    "ModeChanged"
		    #'darkman--mode-changed-signal-handler))
	     (load-theme (darkman-get-theme)))	
	(when (daemonp)
	  (remove-hook 'server-after-make-frame-hook #'darkman-mode)))
    (dbus-unregister-object darkman--dbus-signal)
    (setq darkman--dbus-signal nil)))

(when (daemonp) (add-hook 'server-after-make-frame-hook #'darkman-mode))

(provide 'darkman)
;; darkman.el ends here
