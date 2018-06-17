;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq myOrg-packages
  '(
    company
    smartparens
    (evil-org :location local)
    evil-surround
    gnuplot
    htmlize
    (ob :location built-in)
    (org :location built-in)
    (org-agenda :location built-in)
    org-download
    org-present
    org-gcal
    org-journal
    ))

(defun myOrg/post-init-company ()
  (spacemacs|add-company-hook org-mode)
  (push 'company-capf company-backends-org-mode))

(defun myOrg/post-init-smartparens ()
  (add-hook 'org-mode-hook 'smartparens-mode))

(defun myOrg/init-evil-org ()
  (use-package evil-org
    :commands (evil-org-mode evil-org-recompute-clocks)
    :init (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (progn
      (evil-define-key 'normal evil-org-mode-map
        "O" 'evil-open-above)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "C" 'evil-org-recompute-clocks)
      (spacemacs|diminish evil-org-mode " ⓔ" " e"))))

(defun myOrg/post-init-evil-surround ()
  (defun spacemacs/add-org-surrounds ()
    (push '(?: . spacemacs//surround-drawer) evil-surround-pairs-alist)
    (push '(?# . spacemacs//surround-code) evil-surround-pairs-alist))
  (add-hook 'org-mode-hook 'spacemacs/add-org-surrounds))

(defun myOrg/init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'org-mode
            "tp" 'org-plot/gnuplot)))

(defun myOrg/init-htmlize ()
  (use-package htmlize
    :defer t))

(defun myOrg/init-ob ()
  (use-package ob
    :defer t
    :init
    (progn
      (defun spacemacs//org-babel-do-load-languages ()
        "Load all the languages declared in `org-babel-load-languages'."
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages))
      (add-hook 'org-mode-hook 'spacemacs//org-babel-do-load-languages)
      ;; Fix redisplay of inline images after a code block evaluation.
      (add-hook 'org-babel-after-execute-hook 'spacemacs/ob-fix-inline-images))))

(defun myOrg/init-org ()
  (use-package org
    :defer t
    :commands (orgtbl-mode)
    :init
    (progn
      (setq org-clock-persist-file (concat spacemacs-cache-directory
                                           "org-clock-save.el")
            org-id-locations-file (concat spacemacs-cache-directory
                                          ".org-id-locations")
            org-publish-timestamp-directory (concat spacemacs-cache-directory
                                                    ".org-timestamps/")
            org-log-done t
            org-startup-with-inline-images t
            org-image-actual-width nil
            org-src-fontify-natively t
            org-fontify-whole-heading-line t
	          org-fontify-done-headline t
	          org-fontify-quote-and-verse-blocks t
	          org-src-window-setup 'current-window
	          org-highlight-latex-and-related '(latex)
	          org-log-done 'time
            org-confirm-babel-evaluate nil
            org-src-tab-acts-natively t
            org-src-preserve-indentation t
            ;; this is consistent with the value of
            ;; `helm-org-headings-max-depth'.
            org-imenu-depth 8)

      (add-hook 'org-capture-mode-hook 'delete-other-windows)
      (with-eval-after-load 'org-indent
        (spacemacs|hide-lighter org-indent-mode))
      (let ((dir (configuration-layer/get-layer-local-dir 'org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))
      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      ;; Follow the confirm and abort conventions
      (with-eval-after-load 'org-capture
        (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
          dotspacemacs-major-mode-leader-key 'org-capture-finalize
          "c" 'org-capture-finalize
          "k" 'org-capture-kill
          "a" 'org-capture-kill
          "r" 'org-capture-refile))

      (with-eval-after-load 'org-src
        (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
          dotspacemacs-major-mode-leader-key 'org-edit-src-exit
          "c" 'org-edit-src-exit
          "a" 'org-edit-src-abort
          "k" 'org-edit-src-abort))

      (let ((dir (configuration-layer/get-layer-local-dir 'org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))
      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      ;; Insert key for org-mode and markdown a la C-h k
      ;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
      (defun spacemacs/insert-keybinding-org (key)
        "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
        (interactive "kType key sequence: ")
        (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
          (if (null (equal key "\r"))
              (insert
               (format tag (help-key-description key nil)))
            (insert (format tag ""))
            (forward-char -8))))

      (dolist (prefix '(("me" . "export")
                        ("mx" . "text")
                        ("mh" . "headings")
                        ("mi" . "insert")
                        ("mS" . "subtrees")
                        ("mt" . "tables")
                        ("mtd" . "delete")
                        ("mti" . "insert")
                        ("mtt" . "toggle")))
        (spacemacs/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "'" 'org-edit-special
        "c" 'org-capture
        "d" 'org-deadline
        "D" 'org-insert-drawer
        "ee" 'org-export-dispatch
        "f" 'org-set-effort
        "P" 'org-set-property
        ":" 'org-set-tags

        "a" 'org-agenda
        "b" 'org-tree-to-indirect-buffer
        "A" 'org-archive-subtree
        "l" 'org-open-at-point
        "T" 'org-show-todo-tree

        "." 'org-time-stamp
        "!" 'org-time-stamp-inactive

        ;; headings
        "hi" 'org-insert-heading-after-current
        "hI" 'org-insert-heading
        "hs" 'org-insert-subheading

        ;; More cycling options (timestamps, headlines, items, properties)
        "L" 'org-shiftright
        "H" 'org-shiftleft
        "J" 'org-shiftdown
        "K" 'org-shiftup

        ;; Change between TODO sets
        "C-S-l" 'org-shiftcontrolright
        "C-S-h" 'org-shiftcontrolleft
        "C-S-j" 'org-shiftcontroldown
        "C-S-k" 'org-shiftcontrolup

        ;; Subtree editing
        "Sl" 'org-demote-subtree
        "Sh" 'org-promote-subtree
        "Sj" 'org-move-subtree-down
        "Sk" 'org-move-subtree-up

        ;; tables
        "ta" 'org-table-align
        "tb" 'org-table-blank-field
        "tc" 'org-table-convert
        "tdc" 'org-table-delete-column
        "tdr" 'org-table-kill-row
        "te" 'org-table-eval-formula
        "tE" 'org-table-export
        "th" 'org-table-previous-field
        "tH" 'org-table-move-column-left
        "tic" 'org-table-insert-column
        "tih" 'org-table-insert-hline
        "tiH" 'org-table-hline-and-move
        "tir" 'org-table-insert-row
        "tI" 'org-table-import
        "tj" 'org-table-next-row
        "tJ" 'org-table-move-row-down
        "tK" 'org-table-move-row-up
        "tl" 'org-table-next-field
        "tL" 'org-table-move-column-right
        "tn" 'org-table-create
        "tN" 'org-table-create-with-table.el
        "tr" 'org-table-recalculate
        "ts" 'org-table-sort-lines
        "ttf" 'org-table-toggle-formula-debugger
        "tto" 'org-table-toggle-coordinate-overlays
        "tw" 'org-table-wrap-region

        ;; Multi-purpose keys
        (or dotspacemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
        "*" 'org-ctrl-c-star
        "RET" 'org-ctrl-c-ret
        "-" 'org-ctrl-c-minus
        "^" 'org-sort
        "/" 'org-sparse-tree

        "I" 'org-clock-in
        "n" 'org-narrow-to-subtree
        "N" 'widen
        "O" 'org-clock-out
        "q" 'org-clock-cancel
        "R" 'org-refile
        "s" 'org-schedule

        ;; insertion of common elements
        "ia" 'org-attach
        "il" 'org-insert-link
        "if" 'org-footnote-new
        "ik" 'spacemacs/insert-keybinding-org

        ;; images and other link types have no commands in org mode-line
        ;; could be inserted using yasnippet?
        ;; region manipulation
        "xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
        "xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
        "xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
        "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
        "xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
        "xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
        "xv" (spacemacs|org-emphasize spacemacs/org-verbose ?=))

      ;; Add global evil-leader mappings. Used to access org-agenda
      ;; functionalities – and a few others commands – from any other mode.
      (spacemacs/declare-prefix "ao" "org")
      (spacemacs/set-leader-keys
        ;; org-agenda
        "ao#" 'org-agenda-list-stuck-projects
        "ao/" 'org-occur-in-agenda-files
        "aoa" 'org-agenda-list
        "aoe" 'org-store-agenda-views
        "aom" 'org-tags-view
        "aoo" 'org-agenda
        "aos" 'org-search-view
        "aot" 'org-todo-list
        ;; other
        "aoO" 'org-clock-out
        "aoc" 'org-capture
        "aol" 'org-store-link
        "aoj" 'org-journal-new-entry)

      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (define-key global-map "\C-cc" 'org-capture))
    :config
    (progn
      (setq org-default-notes-file "/users/sanghyuk/Dropbox/org/BeOrg/notes.org")

      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (define-key org-src-mode-map
        (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '"))
        'org-edit-src-exit)

      (spacemacs/set-leader-keys "Cc" 'org-capture)
      (setq org-capture-templates
            '(("i" "Inbox" entry (file "/users/sanghyuk/Dropbox/org/BeOrg/inbox.org")
               "* %?\n:PROPERTIES:\n:CREATED: %u\n:END:")
              ("a" "Appointment" entry (file  "/users/sanghyuk/Dropbox/org/appt_gcal.org")
               "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
              ("h" "Honey" entry (file  "/users/sanghyuk/Dropbox/org/honey_gcal.org")
               "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
              ("s" "School" entry (file  "/users/sanghyuk/Dropbox/org/school_gcal.org")
               "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")))

      ;; Evilify the calendar tool on C-c .
      (unless (eq 'emacs dotspacemacs-editing-style)
        (define-key org-read-date-minibuffer-local-map (kbd "M-h")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-day 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-l")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-day 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-k")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-week 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-j")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-week 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-H")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-month 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-L")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-month 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-K")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-year 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-J")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-year 1))))))))

(defun myOrg/init-org-agenda ()
  (use-package org-agenda
    :defer t
    :init
    (progn
      (setq org-agenda-restore-windows-after-quit t)
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        ":" 'org-agenda-set-tags
        "a" 'org-agenda
        "d" 'org-agenda-deadline
        "f" 'org-agenda-set-effort
        "I" 'org-agenda-clock-in
        "O" 'org-agenda-clock-out
        "P" 'org-agenda-set-property
        "q" 'org-agenda-refile
        "Q" 'org-agenda-clock-cancel
        "s" 'org-agenda-schedule)
      (setq org-directory "~/Dropbox/org")
      (setq org-agenda-files
            '("~/Dropbox/org/BeOrg/inbox.org"
              "~/Dropbox/org/BeOrg/projects.org"
              "~/Dropbox/org/BeOrg/todo.org"
              "~/Dropbox/org/BeOrg/notes.org"
              "~/Dropbox/org/appt_gcal.org"
              "~/Dropbox/org/honey_gcal.org"
              "~/Dropbox/org/school_gcal.org"))
      (setq org-agenda-window-setup (quote current-window))
      (setq org-deadline-warning-days 7)
      (setq org-agenda-skip-scheduled-if-done t)
      (setq org-agenda-skip-deadline-if-done t)
      (spacemacs|define-transient-state org-agenda
      :title "Org-agenda transient state"
      :on-enter (setq which-key-inhibit t)
      :on-exit (setq which-key-inhibit nil)
      :foreign-keys run
      :doc
      "
Headline^^            Visit entry^^               Filter^^                    Date^^               Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------  -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule      [_tf_] follow        [_vd_] day         [_ci_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dd_] set deadline  [_tl_] log           [_vw_] week        [_co_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fc_] by category          [_dt_] timestamp     [_ta_] archive       [_vt_] fortnight   [_ck_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_+_]  do later      [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_hT_] set tags       ^^                          [_fx_] by regexp            [_-_]  do earlier    [_td_] diaries       [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   ^^                   ^^                   [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vr_] reset       ^^             ^^
[_q_] quit
"
      :bindings
      ;; Entry
      ("ht" org-agenda-todo)
      ("hk" org-agenda-kill)
      ("hr" org-agenda-refile)
      ("hA" org-agenda-archive-default)
      ("hT" org-agenda-set-tags)
      ("hp" org-agenda-priority)

      ;; Visit entry
      ("SPC" org-agenda-show-and-scroll-up)
      ("<tab>" org-agenda-goto :exit t)
      ("TAB" org-agenda-goto :exit t)
      ("RET" org-agenda-switch-to :exit t)
      ("o"   link-hint-open-link :exit t)

      ;; Date
      ("ds" org-agenda-schedule)
      ("dd" org-agenda-deadline)
      ("dt" org-agenda-date-prompt)
      ("+" org-agenda-do-date-later)
      ("-" org-agenda-do-date-earlier)

      ;; View
      ("vd" org-agenda-day-view)
      ("vw" org-agenda-week-view)
      ("vt" org-agenda-fortnight-view)
      ("vm" org-agenda-month-view)
      ("vy" org-agenda-year-view)
      ("vn" org-agenda-later)
      ("vp" org-agenda-earlier)
      ("vr" org-agenda-reset-view)

      ;; Toggle mode
      ("tf" org-agenda-follow-mode)
      ("tl" org-agenda-log-mode)
      ("ta" org-agenda-archives-mode)
      ("tr" org-agenda-clockreport-mode)
      ("td" org-agenda-toggle-diary)

      ;; Filter
      ("ft" org-agenda-filter-by-tag)
      ("fr" org-agenda-filter-by-tag-refine)
      ("fc" org-agenda-filter-by-category)
      ("fh" org-agenda-filter-by-top-headline)
      ("fx" org-agenda-filter-by-regexp)
      ("fd" org-agenda-filter-remove-all)

      ;; Clock
      ("ci" org-agenda-clock-in :exit t)
      ("co" org-agenda-clock-out)
      ("ck" org-agenda-clock-cancel)
      ("cj" org-agenda-clock-goto :exit t)

      ;; Other
      ("q" nil :exit t)
      ("gr" org-agenda-redo)
      ("." org-agenda-goto-today)
      ("gd" org-agenda-goto-date)))
    :config
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      (kbd "M-j") 'org-agenda-next-item
      (kbd "M-k") 'org-agenda-previous-item
      (kbd "M-h") 'org-agenda-earlier
      (kbd "M-l") 'org-agenda-later
      (kbd "gd") 'org-agenda-toggle-time-grid
      (kbd "gr") 'org-agenda-redo
      (kbd "M-RET") 'org-agenda-show-and-scroll-up
      (kbd "M-SPC") 'spacemacs/org-agenda-transient-state/body
      (kbd "s-M-SPC") 'spacemacs/org-agenda-transient-state/body)))

(defun myOrg/init-org-download ()
  (use-package org-download
    :commands (org-download-enable
               org-download-yank
               org-download-screenshot)
    :init
    (progn
      (add-hook 'org-mode-hook 'org-download-enable)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "iy" 'org-download-yank
        "is" 'org-download-screenshot))))

(defun myOrg/init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (evilified-state-evilify nil org-present-mode-keymap
        "h" 'org-present-prev
        "l" 'org-present-next
        "q" 'org-present-quit)
      (defun spacemacs//org-present-start ()
        "Initiate `org-present' mode"
        (org-present-big)
        (org-display-inline-images)
        (org-present-hide-cursor)
        (org-present-read-only)
        (evil-evilified-state))
      (defun spacemacs//org-present-end ()
        "Terminate `org-present' mode"
        (org-present-small)
        (org-remove-inline-images)
        (org-present-show-cursor)
        (org-present-read-write)
        (evil-normal-state))
      (add-hook 'org-present-mode-hook 'spacemacs//org-present-start)
      (add-hook 'org-present-mode-quit-hook 'spacemacs//org-present-end))))

(defun myOrg/init-org-gcal ()
  (use-package org-gcal
    :commands (org-gcal-sync)
    :config
    (progn
      (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
      (setq org-gcal-client-id "525899375201-00hs1dks72ihc8vcrj013dv03ssg0qj2.apps.googleusercontent.com"
            org-gcal-client-secret "sosBUGNTGwz4W6Df9VgRKC6T"
            org-gcal-file-alist '(("sanghyuk.park85@gmail.com" .  "~/Dropbox/org/appt_gcal.org")
                                  ("4vqpvq9t0c6ks3m33ljjfecmto@group.calendar.google.com" .  "~/Dropbox/org/honey_gcal.org")
                                  ("fhpns3sdcd9nakpug3kthkp0q0@group.calendar.google.com" .  "~/Dropbox/org/school_gcal.org"))))))

(defun myOrg/init-org-journal ()
  (use-package org-journal
    :commands (org-journal-new-entry)
    :config
    (setq org-journal-dir "~/Dropbox/org/.org/journal/")))
