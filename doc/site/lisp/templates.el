;;; templates.el

;; Copyright (C) 2023  Aziz Ben Ali

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

;; templates.el provides the XML templates of http://grtcdr.tn/darkman.el.

;;; Code:

(require 'shr)

(defalias 'sexp->xml #'shr-dom-to-xml)

(defun templates/stylesheet (href)
  "Format HREF as an HTML stylesheet."
  (sexp->xml
   `(link ((rel . "stylesheet")
	   (href . ,href)))))

(defun templates/main-preamble (_)
  "Define an XML template to be used as a preamble among publishing projects."
  (sexp->xml
   '(nav nil
	 (ul nil
	     (li nil
		 (a ((href . "/darkman.el/index.html"))
		    "Home"))
	     (li nil
		 (a ((href . "/darkman.el/CHANGELOG.html"))
		    "Changelog"))
	     (li nil
		 (a ((href . "/darkman.el/TODO.html"))
		    "To-dos"))
	     (li nil
		 (a ((href . "https://github.com/grtcdr/darkman.el"))
		    "Development"))))))

(defun templates/html-head ()
  "Define an XML template to be included as headers among publishing projects."
  (concat
   (templates/stylesheet "/darkman.el/css/custom.css")
   (templates/stylesheet "https://grtcdr.tn/css/def.css")
   (templates/stylesheet "https://grtcdr.tn/css/common.css")
   (templates/stylesheet "https://grtcdr.tn/css/heading.css")
   (templates/stylesheet "https://grtcdr.tn/css/nav.css")
   (templates/stylesheet "https://grtcdr.tn/css/org.css")
   (templates/stylesheet "https://grtcdr.tn/css/source.css")
   (templates/stylesheet "https://grtcdr.tn/css/table.css")
   (templates/stylesheet "https://grtcdr.tn/css/figure.css")
   (sexp->xml '(link ((rel . "icon")
		      (type . "image/x-icon")
		      (href . "https://grtcdr.tn/assets/favicon.ico"))))))

(provide 'site/templates)
;; templates.el ends here
