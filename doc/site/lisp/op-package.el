;;; op-package.el  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Aziz Ben Ali

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

;; op-package.el provides helper functions to handle the dependencies of
;; https://grtcdr.tn.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun op-package--initialize ()
  "Initialize the package manager and its archives."
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(defun op-package-install (packages)
  "Install the list of PACKAGES."
  (op-package--initialize)
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(op-package-install '(htmlize engrave-faces))

(provide 'op-package)
