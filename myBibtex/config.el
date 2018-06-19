;;; config.el --- myBibtex configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Tell org to use latexmk when compiling the file to pdf
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
