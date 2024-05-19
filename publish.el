;;; publish.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Diego Sejas-Viscarra
;;
;; Author: Diego Sejas-Viscarra <dsejas.math@pm.me>
;; Maintainer: Diego Sejas-Viscarra <dsejas.math@pm.me>
;; Created: May 19, 2024
;; Modified: May 19, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/diego/publish
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'package)
;; Set a local package installation directory
(setq package-user-dir (expand-file-name "./.packages"))
;; Set package archives for installation
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
;; Initialize the package system
(package-initialize)
;; Update the list of packages only if there is not a list already
(unless package-archive-contents
  (package-refresh-contents))
;; use-package make things easier
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Load org's publishing system
(require 'ox-publish)

;; The invariant parts of the site will be programmed in esxml
(use-package esxml
  :pin "melpa-stable"
  :ensure t)

(defun dsv/generate-html-code (title content info)
  "Generate the html code for the page"
  (concat
   "<!DOCTYPE html>"
   (sxml-to-xml
    `(html (@ (lang "en"))
      (head
       (meta (@ (charset "utf-8")))
       (meta (@ (name "author")
                (content "Diego Sejas-Viscarra")))
       (meta (@ (name "viewport")
                (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
       (title ,title))
      (body
       (header
        (nav (@ (class "navbar"))
             (div (@ (class "navbar-left"))
                  (a (@ (class "navbar-link") (href "/")) "")
                  (a (@ (class "navbar-link") (href "/about")) "About me")
                  (a (@ (class "navbar-link") (href "/projects")) "Projects")
                  (a (@ (class "navbar-link") (href "/publications")) "Publications")
                  (a (@ (class "navbar-link") (href "/cv")) "CV")
                  (a (@ (class "navbar-link") (href "/news")) "News"))
             (div (@ (class "navbar-right")))))
       (main ,content))))))

(defun dsv/site-template (content info)
  "Define the general template for the site"
   (dsv/generate-html-code (org-export-data (plist-get info :title) info)
                          content
                          info))

;; Derive the publishing backend from html, but with our custom template
(org-export-define-derived-backend 'site-html 'html
  :translate-alist
  '((template . dsv/site-template)))

;; Redefine org-html-publish-to-html so it uses our custom backend
(defun org-html-publish-to-html (plist filename pubdir)
  (org-publish-org-to 'site-html
                      filename
                      ".html"
                      plist
                      pubdir))

;; Set some global parameters
(setq org-html-validation-link t
      org-export-with-smart-quotes t    ;; render single quotes, double quotes, and apostrophes correctly
      org-html-html5-fancy t            ;; use html5
      org-html-self-link-headlines nil  ;; section titles do not link to themselves
      org-export-with-toc nil)

;; Define the publishing project
(setq org-publish-project-alist
      (list '("diego-sejas:main"
              :recursive t
              :base-directory "./content"
              :publishing-directory "./public"
              :publishing-function org-html-publish-to-html
              :section-numbers nil)))

;; Generate the site
(org-publish-all t)

(message "Build complete!")

(provide 'publish)
;;; publish.el ends here
