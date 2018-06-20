;;; config.el --- myBasic configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Sort Directory first in Dired mode
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

;; Adaptive default screen size
(defun my/default-frame-size ()
  (progn
    (if (> (x-display-pixel-width) 1500)
        (progn
          (add-to-list 'default-frame-alist '(width . 130))
          (setq initial-frame-alist '((left . 300) (top . 10))))
      (progn
        (add-to-list 'default-frame-alist '(width . 100))
        (setq initial-frame-alist '((left . 150) (top . 10)))))
    (add-to-list 'default-frame-alist '(height . 120))))
(my/default-frame-size)

;; Backup setting
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)

;; Line wrapper
(global-visual-line-mode)
(add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

;; Tab width
(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)

;; Customize Solarized-theme
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(defun customize-solarized ()
  "Customize solarized theme"
  (if (member 'solarized-dark custom-enabled-themes)
      (custom-theme-set-faces
       'solarized-dark
       ;; Org block
       '(org-block-begin-line ((t (:foreground "#9EA0E5":background "#073642"))))
       '(org-block-end-line ((t (:foreground "#9EA0E5":background "#073642"))))
       ;; Power line
       '(powerline-active1 ((t (:background "#5D4D7A" :foreground "#b2b2b2"))))
       '(powerline-active2 ((t (:background "#5D4D7A" :foreground "#b2b2b2"))))
       '(powerline-inactive1 ((t (:background "#073642" :foreground "#586e75"))))
       '(powerline-inactive2 ((t (:background "#073642" :foreground "#586e75"))))
       ;; Linum-relative current line
       '(linum-relative-current-face ((t (:foreground "#839496"
                                                      :underline nil
                                                      :weight normal
                                                      :background "#002b36"))))
       ))
  (if (member 'solarized-light custom-enabled-themes)
      (custom-theme-set-faces
       'solarized-light
       ;; Org block
       '(org-block-begin-line ((t (:foreground "#3f4d91":background "#eee8d5"))))
       '(org-block-end-line ((t (:foreground "#3f4d91":background "#eee8d5"))))
       ;; Power line
       '(powerline-active1 ((t (:background "#D3D3E7" :foreground "#655370"))))
       '(powerline-active2 ((t (:background "#D3D3E7" :foreground "#655370"))))
       '(powerline-inactive1 ((t (:background "#EFEAE9" :foreground "#655370"))))
       '(powerline-inactive2 ((t (:background "#EFEAE9" :foreground "#655370"))))
       ;; Linum-relative current line
       '(linum-relative-current-face ((t (:foreground "#657b83"
                                                      :underline nil
                                                      :weight normal
                                                      :background "#fdf6e3"))))
       )))
(customize-solarized)
(add-hook 'after-load-theme-hook 'customize-solarized)

;; Set up jump to registers
(spacemacs/set-leader-keys "jr" 'jump-to-register)
(set-register ?i '(file . "~/Dropbox/org/BeOrg/inbox.org"))
(set-register ?b '(file . "~/Dropbox/Bibliography/references.bib"))
(set-register ?p '(file . "~/Dropbox/org/BeOrg/projects.org"))
(set-register ?n '(file . "~/Dropbox/org/BeOrg/notes.org"))
(set-register ?t '(file . "~/Dropbox/org/BeOrg/todo.org"))
