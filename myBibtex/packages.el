;;; packages.el --- BibTeX Layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Joshua Ellis <josh@jpellis.me>
;; URL: https://github.com/JP-Ellis
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst myBibtex-packages
      '(
        auctex
        helm-bibtex
        org
        org-ref
        markdown-mode
        ))

(defun myBibtex/post-init-auctex ()
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "ic" 'org-ref-helm-insert-cite-link))

(defun myBibtex/init-helm-bibtex ()
  (use-package helm-bibtex
    :commands (helm-bibtex)
    :init
    (progn
      (spacemacs/set-leader-keys "ob" 'helm-bibtex)
      (setq helm-bibtex-full-frame nil
            bibtex-completion-bibliography '("~/Dropbox/Bibliography/references.bib")
            bibtex-completion-notes-path "~/Dropbox/Bibliography/Notes/"
            bibtex-completion-library-path '("~/Dropbox/Bibliography/Papers/"))
      (setq bibtex-completion-pdf-open-function
            (lambda (fpath)
              (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath)))
      (setq bibtex-completion-pdf-symbol "f")
      (setq bibtex-completion-notes-symbol "n")
      (setq bibtex-completion-display-formats
            '((t . "${=type=:7}: ${author: 20} (${year:4}) ${title:*} :${=has-pdf=:1}${=has-note=:1}:"))))
    :config
    (setq helm-bibtex-notes-template-multiple-files
          (format
           "#+TITLE: Notes on: ${title}\n#+INTERLEAVE_PDF: ~/Dropbox/Bibliography/Papers/${=key=}.pdf\n#+PUB_AUTHORS:\t${author}\n#+PUB_YEAR: \t${year}\n\n"))))

(defun myBibtex/post-init-helm-bibtex ()
  (with-eval-after-load 'helm-bibtex
    (progn
      (require 'org-ref)
      (setq bibtex-completion-display-formats
            '((t . "${=type=:7}: ${author: 20} (${year:4}) ${title:*} :${=has-pdf=:1}${=has-note=:1}:"))))))

(defun myBibtex/post-init-org ()
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "ic" 'org-ref-helm-insert-cite-link))

(defun myBibtex/init-org-ref ()
  (use-package org-ref
    :defer t
    ;; :commands (org-ref-helm-insert-cite-link
    ;;            org-ref-bibtex-next-entry
    ;;            org-ref-bibtex-previous-entry
    ;;            org-ref-open-in-browser
    ;;            org-ref-open-bibtex-notes
    ;;            org-ref-open-bibtex-pdf
    ;;            org-ref-bibtex-hydra/body
    ;;            org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
    ;;            org-ref-sort-bibtex-entry
    ;;            arxiv-add-bibtex-entry
    ;;            arxiv-get-pdf-add-bibtex-entry
    ;;            doi-utils-add-bibtex-entry-from-doi
    ;;            isbn-to-bibtex
    ;;            pubmed-insert-bibtex-from-pmid)
    :init
    (progn
      (evil-define-key 'normal bibtex-mode-map
        (kbd "C-j") 'org-ref-bibtex-next-entry
        (kbd "C-k") 'org-ref-bibtex-previous-entry
        "gj" 'org-ref-bibtex-next-entry
        "gk" 'org-ref-bibtex-previous-entry)

      (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
        ;; Navigation
        "j" 'org-ref-bibtex-next-entry
        "k" 'org-ref-bibtex-previous-entry

        ;; Open
        "b" 'org-ref-open-in-browser
        "n" 'org-ref-open-bibtex-notes
        "p" 'org-ref-open-bibtex-pdf

        ;; Misc
        "h" 'org-ref-bibtex-hydra/body
        "i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
        "s" 'org-ref-sort-bibtex-entry

        ;; Lookup utilities
        "la" 'arxiv-add-bibtex-entry
        "lA" 'arxiv-get-pdf-add-bibtex-entry
        "ld" 'doi-utils-add-bibtex-entry-from-doi
        "li" 'isbn-to-bibtex
        "lp" 'pubmed-insert-bibtex-from-pmid))
    :config
    (progn
      (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
      (setq org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
            org-ref-pdf-directory "~/Dropbox/bibliography/papers/"
            org-ref-notes-directory "~/Dropbox/Bibliography/Notes/")
      (setq org-ref-notes-function
            (lambda (thekey)
              (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
                (bibtex-completion-edit-notes
                 (list (car (org-ref-get-bibtex-key-and-file thekey)))))))
      (setq org-ref-open-pdf-function
            (lambda (fpath)
              (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath))))))

(defun myBibtex/post-init-markdown-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
    "ic" 'org-ref-helm-insert-cite-link))
