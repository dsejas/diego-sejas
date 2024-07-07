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

;; Redefine org-html-toc
(defun org-html-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (org-html--format-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      `(nav (@ (class "toc"))
        ;; (h2 (@ (class "toc-heading")) ,(org-html--translate "Table of Contents" info))
             ,(org-html--toc-text toc-entries)))))

(defun dsv/site-header-en ()
  "Generate the html code for the site header"
  (list `(header
          (nav (@ (class "navbar"))
               (div (@ (class "navbar-left"))
                    (a (@ (class "navbar-item") (href "/"))
                       (span (@ (class "fas fa-house navbar-icon")) ""))
                    (a (@ (class "navbar-item") (href "/en/about"))
                       (span (@ (class "fas fa-info-circle navbar-icon")) "") "About me")
                    (div (@ (class "navbar-item menu"))
                         (span (@ (class "fas fa-tools navbar-icon")) "")
                         (span (@ (class "menu-title")) "Projects ")
                         (span (@ (class "fas fa-caret-down")) "")
                         (div (@ (class "menu-content"))
                              (a (@ (class "menu-item") (href "/mathematics")) "Mathematics")
                              (a (@ (class "menu-item") (href "/software")) "Software")
                              (a (@ (class "menu-item") (href "/latex")) "LaTeX")
                              (a (@ (class "menu-item") (href "/translations")) "Translations")))
                    (a (@ (class "navbar-item") (href "/en/publications"))
                       (span (@ (class "fas fa-pen-nib navbar-icon")) "") "Publications")
                    (a (@ (class "navbar-item") (href "/en/cv"))
                       (span (@ (class "fas fa-graduation-cap navbar-icon")) "") "CV")
                    (a (@ (class "navbar-item") (href "/en/news"))
                       (span (@ (class "fas fa-bullhorn navbar-icon")) "") "News"))
               (div (@ (class "navbar-right"))
                    (a (@ (class "navbar-item")
                          (href "https://orcid.org/0000-0002-0368-2161"))
                       (span (@ (class "fa-brands fa-orcid")) ""))
                    (a (@ (class "navbar-item")
                          (href "https://www.researchgate.net/profile/Diego-Sejas-Viscarra-2"))
                       (span (@ (class "fa-brands fa-researchgate")) ""))
                    (a (@ (class "navbar-item")
                          (href "https://www.researchgate.net/profile/Diego-Sejas-Viscarra-2"))
                       (span (@ (class "fa-brands fa-linkedin")) ""))
                    (a (@ (class "navbar-item underlined") (href "/en")) "English")
                    (a (@ (class "navbar-item") (href "/es")) "Español"))))))

(defun dsv/site-footer-en ()
  "Generate the html code for the site footer"
  (list `(footer
         (div (@ (class "site-footer"))
              (div (@ (class "footer-left")) (i (@ (class "fa-solid fa-copyright")) "") "&nbsp; Diego Sejas")
              (div (@ (class "footer-right"))
                   (a (@ (class "navbar-item") (href "/credits.html")) "Credits"))))))

(defun dsv/generate-html-code-en (title content info)
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
       (link (@ (rel "stylesheet")
                (href "/assets/css/site.css")))
       (link (@ (rel "stylesheet")
                (href "/assets/css/animations.css")))
       (link (@ (rel "stylesheet")
                (href "/assets/css/fontawesome.min.css")))
       (link (@ (rel "stylesheet")
                (href "/assets/css/solid.min.css")))
       (link (@ (rel "stylesheet")
                (href "/assets/css/brands.min.css")))
       (title ,(concat "Diego Sejas | "
        (if-let ((html-title (plist-get info :html-title))) html-title title))))
      (body
       (input (@ (type "checkbox") (id "theme-switch")))
       (div (@ (id "themed"))
            (label (@ (for "theme-switch") (id "switch-label")) "")
            ,@(dsv/site-header-en)
            ,(org-html-toc 2 info)
            (main
             ,(when-let ((main-class (plist-get info :html-main-class))) `(@ (class ,main-class)))
             ,(when-let ((title (plist-get info :title))) `(h1 ,@title))
             ,content)
            ,@(dsv/site-footer-en)))))))


(defun dsv/site-template (content info)
  "Define the general template for the site"
   (dsv/generate-html-code-en (org-export-data (plist-get info :title) info)
                          content
                          info))

;; Derive the publishing backend from html, but with our custom template
(org-export-define-derived-backend 'site-html 'html
  :translate-alist
  '((template . dsv/site-template))
  :options-alist
  '((:html-title "HTML-TITLE" nil nil nil)
    (:html-main-class "HTML-MAIN-CLASS" nil nil nil)))

(defun dsv/get-output-dir (filename pubdir)
  (if (string-match "\\/index.org\\|\\/404.org$" filename)
      pubdir
    (let ((output-dir
           (concat pubdir
                   (downcase
                    (file-name-as-directory
                     (file-name-sans-extension
                      (file-name-nondirectory filename)))))))
      (unless (file-directory-p output-dir)
        (make-directory output-dir t))
      output-dir)))

;; (defun org-export-output-file-name (extension &optional subtreep pub-dir)
;;   (let ((output-dir (dsv/get-output-dir filename pub-dir)))
;;     (concat output-dir
;;             (if (string= (file-name-nondirectory filename) "404.org") "404" "index")
;;             extension)))

;; Redefine org-html-publish-to-html so it uses our custom backend
(defun org-html-publish-to-html (plist filename pubdir)
  (let ((output-dir (dsv/get-output-dir filename pubdir)))
    (cl-letf (((symbol-function 'org-export-output-file-name)
               (lambda (extension &optional subtreep pubdir)
                 (concat output-dir
                         (if (string= (file-name-nondirectory filename) "404.org")
                             "404"
                           "index")
                         extension))))
      (org-publish-org-to 'site-html
                          filename
                          (if (string= (file-name-nondirectory filename) "404.org")
                              ".shtml"  ;; godaddy uses requires this for 404 pages to work
                            ".html")
                          plist
                          pubdir))))

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
              :section-numbers nil)
            '("diego-sejas:assets"
              :recursive t
              :base-directory "./assets"
              :base-extension "css\\|woff2"
              :publishing-directory "./public/assets"
              :publishing-function org-publish-attachment)))

(copy-file "./content/index.org" "./content/en/index.org" t)

;; Generate the site
(org-publish-all t)

(message "Build complete!")

(provide 'publish)
;;; publish.el ends here
