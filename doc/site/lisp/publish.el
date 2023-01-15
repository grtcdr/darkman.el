;;; publish.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aziz Ben Ali

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

;; publish.el is the publishing specification of http://grtcdr.tn/darkman.el.

;;; Code:

(add-to-list 'load-path (concat default-directory "lisp/"))

(require 'templates)
(require 'ox-publish)
(require 'project)

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn")

(setq org-publish-timestamp-directory ".cache/"
      org-src-fontify-natively nil
      org-html-preamble nil
      org-html-postamble nil
      org-html-doctype "html5"
      org-html-htmlize-output-type nil
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil)

(setq org-publish-project-alist
      (list
       (list "main"
	     :base-extension "org"
	     :base-directory "src/"
	     :publishing-directory "public/"
	     :publishing-function 'org-html-publish-to-html
	     :exclude "handbook.org"
	     :html-preamble 'site/main-preamble
	     :html-postamble nil
	     :html-head site/html-head
	     :with-toc nil)
       (list "handbook"
	     :base-extension "org"
	     :base-directory "src/"
	     :publishing-directory "public/"
	     :exclude ".*"
	     :include '("handbook.org")
	     :publishing-function 'org-latex-publish-to-latex
	     :with-author t
	     :with-email t)
       (list "css"
	     :base-extension "css"
	     :base-directory "src/css/"
	     :publishing-directory "public/css/"
	     :publishing-function 'org-publish-attachment)
       (list "all"
	     :components '("css" "handbook" "main"))))
