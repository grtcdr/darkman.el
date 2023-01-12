;;; publish.el --- A minimal publishing script     -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://github.com/grtcdr/darkman.el

;; publish.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; publish.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with publish.el. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ox-publish)
(require 'project)
(require 'shr)

(defun site/link (href)
  "Format as a ’link’ tag, a resource located at HREF with a
relationship of REL."
  (shr-dom-to-xml
   `(link ((rel . "stylesheet")
	   (href . ,href)))))

(defun site/stylesheet (filename)
  "Format filename as a stylesheet."
  (shr-dom-to-xml
   `(link ((rel . "stylesheet")
	   (href . ,filename)))))

(defvar site/html-head
  (concat
   (site/stylesheet "https://grtcdr.tn/css/def.css")
   (site/stylesheet "https://grtcdr.tn/css/common.css")
   (site/stylesheet "https://grtcdr.tn/css/heading.css")
   (site/stylesheet "https://grtcdr.tn/css/nav.css")
   (site/stylesheet "https://grtcdr.tn/css/org.css")
   (site/stylesheet "https://grtcdr.tn/css/indent.css")
   (site/stylesheet "https://grtcdr.tn/css/source.css")
   (site/stylesheet "https://grtcdr.tn/css/table.css")
   (site/stylesheet "https://grtcdr.tn/css/figure.css")
   (shr-dom-to-xml '(link ((rel . "icon")
			   (type . "image/x-icon")
			   (href . "https://grtcdr.tn/assets/favicon.ico")))))
  "HTML headers shared across projects.")

(defvar main-preamble
  (shr-dom-to-xml
   '(nav nil
	 (ul nil
	     (li nil
		 (a ((href . "/darkman.el/index.html")) "Home"))
	     (li nil
		 (a ((href . "/darkman.el/TODO.html"))
		    "To-dos"))
	     (li nil
		 (a ((href . "https://github.com/grtcdr/darkman.el"))
		    "Development")))))
      "Define an HTML snippet/template used as a preamble across all
projects.")

(setq org-publish-timestamp-directory ".cache/"
      org-src-fontify-natively nil
      org-html-preamble nil
      org-html-postamble nil
      org-html-doctype "html5"
      org-html-htmlize-output-type nil
      org-html-head-include-default-style nil)

(setq org-publish-project-alist
      (list
       (list "main"
	     :base-extension "org"
	     :base-directory "src/"
	     :publishing-directory "public/"
	     :publishing-function 'org-html-publish-to-html
	     :html-preamble main-preamble
	     :html-postamble nil
	     :html-head site/html-head
	     :with-toc nil)
       (list "all"
	     :components '("main"))))
