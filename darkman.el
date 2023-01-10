;;; darkman.el --- Darkman integration with Emacs     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Keywords: convenience

(defvar darkman-dbus-service "nl.whynothugo.darkman")
(defvar darkman-dbus-path "/nl/whynothugo/darkman")
(defvar darkman-dbus-interface darkman-dbus-service)
(defvar darkman-themes '(:light 'modus-operandi :dark 'modus-vivendi))

(defun darkman-get-mode ()
  "Get the mode of the darkman service."
  (interactive)
  (dbus-get-property :session
		     darkman-dbus-service
		     darkman-dbus-path
		     darkman-dbus-interface
		     "Mode"))

(defun darkman-set-mode (mode)
  "Set the mode of the darkman service to MODE."
  (dbus-set-property :session
		     darkman-dbus-service
		     darkman-dbus-path
		     darkman-dbus-interface
		     "Mode" (cond ((member mode '(dark light))
				   (symbol-name mode))
				  (t (error "Not a valid mode.")))))

(defun darkman-toggle ()
  "Toggle the mode of the darkman service."
  (interactive)
  (let ((mode (darkman-get-mode)))
    (cond ((string= mode "dark") (darkman-set-mode 'light))
	  ((string= mode "light") (darkman-set-mode 'dark)))))


(defun darkman-get-theme ()
  "Get the theme corresponding to the mode of the darkman service
from the ‘darkman-themes’ variable."
  (let ((mode (darkman-get-mode)))
    (cond ((string= mode "dark") (plist-get darkman-themes :dark))
	  ((string= mode "light") (plist-get darkman-themes :light)))))

(provide 'darkman)
;; darkman.el ends here
