;;; darkman.el --- Darkman integration with Emacs     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: http://github.com/grtcdr/darkman.el
;; Version: 0.1
;; Keywords: convenience

(require 'dbus)

(defvar darkman-themes '(:light modus-operandi :dark modus-vivendi))
(defvar darkman--dbus-service "nl.whynothugo.darkman")
(defvar darkman--dbus-path (car (last (dbus-introspect-get-all-nodes :session darkman--dbus-service "/"))))
(defvar darkman--dbus-interface darkman--dbus-service)
(defvar darkman--dbus-signal nil)

(defun darkman--invalid-mode-error (mode)
  "Signal an error about an invalid mode. MODE is the name of the invalid mode."
  (error (format "‘%s’ is not a valid mode." mode)))

(defun darkman--dbus-service-unavailable-error ()
  "Signal an error about an invalid mode. MODE is the name of the invalid mode."
  (error (format "%s D-Bus service not available."
		 darkman--dbus-service)))

(defun darkman-get ()
  "Get the mode of the darkman service."
  (interactive)
  (let ((mode (dbus-get-property :session
		     darkman--dbus-service
		     darkman--dbus-path
		     darkman--dbus-interface
		     "Mode")))
    (when (called-interactively-p) (message (format "Mode is currently set to %s." mode)))
    mode))

(defun darkman-set (mode)
  "Set the mode of the darkman service to MODE. MODE can be ‘dark’
or ‘light’."
  (dbus-set-property :session
		     darkman--dbus-service
		     darkman--dbus-path
		     darkman--dbus-interface
		     "Mode" (cond ((member mode '(dark light))
				   (symbol-name mode))
				  (t (darkman--invalid-mode-error mode)))))

(defun darkman-toggle-mode-of-service ()
  "Toggle the mode of the darkman service."
  (interactive)
  (let ((mode (darkman-get)))
    (cond ((string= mode "dark") (darkman-set 'light))
	  ((string= mode "light") (darkman-set 'dark)))))

(defun darkman--get-assoc-theme (mode)
  "Return a theme from ‘darkman-themes’ which corresponds to THEME."
  (cond ((string= mode "dark") (plist-get darkman-themes :dark))
	((string= mode "light") (plist-get darkman-themes :light))
	(t (darkman--invalid-mode-error mode))))

(defun darkman-get-theme ()
  "Get a theme from the ‘darkman-themes’ which corresponds to the current mode."
  (let ((mode (darkman-get)))
    (darkman--get-assoc-theme mode)))

(defun darkman--mode-changed-signal-handler (new-mode)
  "Signal handler for the ModeChanged signal."
  (message "Darkman changed its mode, the theme will be changed.")
  (load-theme (darkman--assoc-theme new-mode)))

(defun darkman--check-dbus-service ()
  "Return non-nil if the darkman service is available."
  (or (dbus-ping :session darkman--dbus-service 100)
      (darkman--dbus-service-unavailable-error)))

(define-minor-mode darkman-mode
  "Minor mode providing integration with the darkman utility."
  :global t
  :init-value nil
  :require 'dbus
  :version "0.1"
  (if darkman-mode
      (and (darkman--check-dbus-service)
	   (setq darkman--dbus-signal
		 (dbus-register-signal :system
				       darkman--dbus-service
				       darkman--dbus-path
				       darkman--dbus-interface
				       "ModeChanged"
				       #'darkman--mode-changed-signal-handler)))
    (setq darkman--dbus-signal nil)))

(provide 'darkman)
;; darkman.el ends here
