;;; darkman.el --- Seamless integration with Darkman  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Maintainer: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://grtcdr.tn/darkman.el/
;; Version: 1.0.0
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

(defgroup darkman ()
  "Seamless integration with Darkman."
  :group 'convenience)

(defcustom darkman-themes
  '(:light modus-operandi :dark modus-vivendi)
  "A property list which maps modes to their corresponding themes.

The two properties, ‘:light’ and ‘:dark’, expect as a value a
symbol representing the name of the theme."
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
(defvar darkman--dbus-monitor nil)

;;;###autoload
(defun darkman-current-mode (&optional message)
  "Return the mode of the Darkman service.
When MESSAGE is non-nil, print the current mode to the echo area instead."
  (interactive (list t))
  (let ((mode (dbus-get-property :session
		     darkman--dbus-service
		     darkman--dbus-path
		     darkman--dbus-interface
		     "Mode")))
    (if message
	(message "Mode is currently set to %s." mode)
      mode)))

(defun darkman-set-mode (mode)
  "Set the mode of the Darkman service to MODE.
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
  "Toggle the mode of the Darkman service."
  (interactive)
  (let ((mode (darkman-current-mode)))
    (cond ((string= mode "dark") (darkman-set-mode 'light))
	  ((string= mode "light") (darkman-set-mode 'dark)))))

(defun darkman--invalid-mode-error (mode)
  "Signal an error about an invalid mode.  MODE is the name of the invalid mode."
  (error "‘%s’ is not a valid mode." mode))

(defun darkman--dbus-service-unavailable-error ()
  "Signal an error about the service being unavailable."
  (error "%s D-Bus service not available." darkman--dbus-service))

(defun darkman--lookup-theme (mode)
  "Return a theme from ‘darkman-themes’ that corresponds to string MODE."
  (cond ((string= mode "dark") (plist-get darkman-themes :dark))
	((string= mode "light") (plist-get darkman-themes :light))
	(t (darkman--invalid-mode-error mode))))

(defun darkman--event-handler (interface property value)
  "Callback function for handling a change in mode.
INTERFACE is the name of the interface that is the target of the event.
PROPERTY is the property that is modified by the event.
VALUE is the new value of PROPERTY."
  (when-let* (((string-equal interface darkman--dbus-service))
	      ((string-equal property "Mode"))
	      (mode (car value))
	      (theme (darkman--lookup-theme mode)))
    (unless darkman-switch-themes-silently
      (message "Darkman switched to %s mode, switching to %s theme."
	       mode theme))
    (load-theme theme)))

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
	     (unless darkman--dbus-monitor
	       (setq darkman--dbus-monitor
		     (dbus-register-monitor
		      :session
		      #'darkman--event-handler
		      :type "method_call"
		      :destination darkman--dbus-service
		      :path darkman--dbus-path
		      :interface "org.freedesktop.DBus.Properties"
		      :member "Set")))
	     (load-theme (darkman--lookup-theme (darkman-current-mode))))
	(when (daemonp)
	  (remove-hook 'server-after-make-frame-hook #'darkman-mode)))
    (dbus-unregister-object darkman--dbus-monitor)
    (setq darkman--dbus-monitor nil)))

(provide 'darkman)
;;; darkman.el ends here
