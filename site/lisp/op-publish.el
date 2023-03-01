;;; op-publish.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://github.com/grtcdr/darkman.el

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

;; op-publish.el defines the specification of https://grtcdr.tn/darkman.el
;; and should be used in conjunction with a build system.

;;; Code:

(add-to-list 'load-path (file-name-concat default-directory "lisp/"))

(require 'ox-publish)
(require 'op-template)
(require 'op-package)

(setq user-full-name "Aziz Ben Ali"
      org-publish-timestamp-directory ".cache/"
      org-src-fontify-natively t
      org-latex-src-block-backend 'engraved
      org-html-preamble nil
      org-html-postamble nil
      org-html-htmlize-output-type nil
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-htmlize-output-type 'css)

(defun op-publish-manual-function (plist filename pub-dir)
  "Call the publishing functions used by the manual."
  (org-latex-publish-to-latex plist filename pub-dir)
  (org-html-publish-to-html plist filename pub-dir))

(setq org-publish-project-alist
      (list
       (list "root"
	     :base-extension "org"
	     :base-directory "src"
	     :publishing-directory "public"
	     :publishing-function 'org-html-publish-to-html
	     :with-toc nil
	     :section-numbers nil
	     :html-head op-template-metadata
	     :html-preamble op-template-navbar)
       (list "manual"
	     :base-extension "org"
	     :base-directory "../doc"
	     :publishing-directory "public"
	     :publishing-function 'op-publish-manual-function
	     :include '("darkman.org")
	     :exclude ".*"
	     :with-author t
	     :with-date nil
	     :html-head op-template-metadata
	     :html-preamble op-template-navbar)
       (list "meta"
	     :base-extension "org"
	     :base-directory ".."
	     :publishing-directory "public"
	     :publishing-function 'org-html-publish-to-html
	     :include '("CHANGELOG.org" "TODO.org")
	     :exclude ".*"
	     :with-toc nil
	     :section-numbers nil
	     :html-head op-template-metadata
	     :html-preamble op-template-navbar)
       (list "all"
	     :components '("root" "meta" "manual"))))
