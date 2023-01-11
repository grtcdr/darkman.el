;;; darkman.el --- Darkman integration with Emacs     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Keywords: convenience

(require 'dbus)

(defvar darkman--dbus-service "nl.whynothugo.darkman")
(defvar darkman--dbus-path (car (last (dbus-introspect-get-all-nodes :session darkman--dbus-service "/"))))
(defvar darkman--dbus-interface darkman--dbus-service)
(defvar darkman-themes '(:light modus-operandi :dark modus-vivendi))

(defun darkman--invalid-mode-error (mode)
    "Signal an error about an invalid mode. MODE is the name of the invalid mode."
    (error (format "‘%s’ is not a valid mode." mode)))

(defun darkman-get-mode ()
  "Get the mode of the darkman service."
  (interactive)
  (dbus-get-property :session
		     darkman--dbus-service
		     darkman--dbus-path
		     darkman--dbus-interface
		     "Mode"))

(defun darkman-set-mode (mode)
  "Set the mode of the darkman service to MODE. MODE can be ‘dark’
or ‘light’."
  (dbus-set-property :session
		     darkman--dbus-service
		     darkman--dbus-path
		     darkman--dbus-interface
		     "Mode" (cond ((member mode '(dark light))
				   (symbol-name mode))
				  (t (darkman--invalid-mode-error mode)))))

(defun darkman-toggle ()
  "Toggle the mode of the darkman service."
  (interactive)
  (let ((mode (darkman-get-mode)))
    (cond ((string= mode "dark") (darkman-set-mode 'light))
	  ((string= mode "light") (darkman-set-mode 'dark)))))

(defun darkman--get-assoc-theme (mode)
  "Return a theme from ‘darkman-themes’ which corresponds to THEME."
  (cond ((string= mode "dark") (plist-get darkman-themes :dark))
	((string= mode "light") (plist-get darkman-themes :light))
	(t (darkman--invalid-mode-error mode))))

(defun darkman-get-theme ()
  "Get a theme from the ‘darkman-themes’ which corresponds to the current mode."
  (let ((mode (darkman-get-mode)))
    (darkman--get-assoc-theme mode)))

(defun darkman--mode-changed-signal-handler (new-mode)
  "Signal handler for the ModeChanged signal."
  (message "Darkman changed its mode, the theme will be changed.")
  (load-theme (darkman--assoc-theme new-mode)))

(defun darkman--check-dbus-service ()
  "Return non-nil if the darkman service is available."
  (or (dbus-ping :session darkman--dbus-service 100)
      (error (format "%s D-Bus service not available, automatic theme-switching will depend on standard darkman scripts."
		     darkman--dbus-service))))

(define-minor-mode darkman-mode
  "Minor mode providing integration with the darkman utility."
  :global t
  :init-value nil
  :require 'dbus
  :version "0.1"
  (and (darkman--check-dbus-service)
       (dbus-register-signal :system
			     darkman--dbus-service
			     darkman--dbus-path
			     darkman--dbus-interface
			     "ModeChanged"
			     #'darkman--mode-changed-signal-handler)))

(provide 'darkman)
;; darkman.el ends here
