;;; packages.el --- myIpython Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq myIpython-packages
      '(
        ob-ipython
        org
        (ox-ipynb :location local)
        ))

(defun myIpython/pre-init-ob-ipython ()
  (setq scimax-dir "path/to/scimax")
  (add-to-list 'load-path "path/to/scimax"))

(defun myIpython/init-ob-ipython ()
    (use-package ob-ipython
      :defer t
      :init
      (with-eval-after-load 'org
        (progn
          (require 'scimax-org-babel-ipython-upstream)
          (require 'ob-ipython)))))

(defun myIpython/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(ipython . t))))

;; (defun myIpython/post-init-ob-ipython ()
;;   (with-eval-after-load 'org
;;     (progn
;;       (require 'scimax-org-babel-ipython-upstream)
;;       (require 'ob-ipython))))

(defun myIpython/init-ox-ipynb ()
  (use-package ox-ipynb
    :defer t
    :init
    (with-eval-after-load 'org (require 'ox-ipynb))))


