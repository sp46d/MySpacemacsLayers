;;; packages.el --- mySolarizedTheme layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sanghyuk Park <sanghyuk@Sanghyuks-MBP.domain>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defconst myBasic-packages
  '(reveal-in-osx-finder
    interleave))

(defun myBasic/init-reveal-in-osx-finder ()
  (use-package reveal-in-osx-finder
    ;; :commands (reveal-in-osx-finder)
    :init
    (progn
      (spacemacs/declare-prefix "o" "my-prefix")
      (spacemacs/set-leader-keys "oo" 'reveal-in-osx-finder))))

(defun myBasic/init-interleave ()
  (use-package interleave
    :commands (interleave-mode)))
