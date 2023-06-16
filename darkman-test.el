;;; darkman-test.el --- Unit tests for darkman.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'darkman)

(defmacro darkman-with-themes (&rest body)
  "Evaluate BODY with a predefined ‘darkman-themes’ value."
  (declare (indent nil))
  `(let ((darkman-themes '(:light tango :dark tango-dark)))
     ,@body))

(ert-deftest darkman-test-lookup-theme ()
  "Test the return value of ‘darkman--lookup-theme’."
  (darkman-with-themes
   (should (equal (darkman--lookup-theme "dark") 'tango-dark))
   (should (equal (darkman--lookup-theme "light") 'tango))
   (should (equal (darkman--lookup-theme "dim") nil))))

(ert-deftest darkman-test-lookup-mode ()
  (darkman-with-themes
   (should (string-equal (darkman--lookup-mode "tango") "light"))
   (should (string-equal (darkman--lookup-mode "tango-dark") "dark"))
   (should (equal (darkman--lookup-mode "modus-vivendi") nil))))
