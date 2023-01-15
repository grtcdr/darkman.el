;;; darkman.el --- Seamless integration between Darkman and Emacs     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: http://github.com/grtcdr/darkman.el
;; Version: 0.1
;; Keywords: convenience

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
  :package-version '(darkman . "0.2.0"))

(defvar darkman--dbus-service "nl.whynothugo.darkman")
(defvar darkman--dbus-path "/nl/whynothugo/darkman")
(defvar darkman--dbus-interface darkman--dbus-service)
(defvar darkman--dbus-signal nil)

(defun darkman-get ()
  "Get the mode of the darkman service."
  (interactive)
  (let ((mode (dbus-get-property :session
				 darkman--dbus-service
				 darkman--dbus-path
				 darkman--dbus-interface
				 "Mode")))
    (when (called-interactively-p)
      (message (format "Mode is currently set to %s." mode)))
    mode))

;;;###autoload
(defun darkman-set (mode)
  "Set the mode of the darkman service to MODE, which can either be
‘dark’ or ‘light’."
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
  (let ((mode (darkman-get)))
    (cond ((string= mode "dark") (darkman-set 'light))
	  ((string= mode "light") (darkman-set 'dark)))))

(defun darkman--invalid-mode-error (mode)
  "Signal an error about an invalid mode. MODE is the name of the invalid mode."
  (error (format "‘%s’ is not a valid mode." mode)))

(defun darkman--dbus-service-unavailable-error ()
  "Signal an error about an invalid mode. MODE is the name of the invalid mode."
  (error (format "%s D-Bus service not available."
		 darkman--dbus-service)))

(defun darkman--get-assoc-theme (mode)
  "Return a theme from ‘darkman-themes’ corresponding to MODE (a string)."
  (cond ((string= mode "dark") (plist-get darkman-themes :dark))
	((string= mode "light") (plist-get darkman-themes :light))
	(t (darkman--invalid-mode-error mode))))

;;;###autoload
(defun darkman-get-theme ()
  "Get a theme from the ‘darkman-themes’ which corresponds to the current mode."
  (let ((mode (darkman-get)))
    (darkman--get-assoc-theme mode)))

(defun darkman--mode-changed-signal-handler (new-mode)
  "Signal handler for the ModeChanged signal."
  (let ((new-theme (darkman--get-assoc-theme new-mode)))
    (unless (not darkman-switch-themes-silently)
      (message (format "Darkman switched to %s mode, switching to %s theme."
		       new-mode new-theme)))
    (load-theme new-theme)))

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
		 (dbus-register-signal :session
				       darkman--dbus-service
				       darkman--dbus-path
				       darkman--dbus-interface
				       "ModeChanged"
				       #'darkman--mode-changed-signal-handler))
           (load-theme (darkman-get-theme)))
    (dbus-unregister-object darkman--dbus-signal)
    (setq darkman--dbus-signal nil)))

(provide 'darkman)
;; darkman.el ends here
