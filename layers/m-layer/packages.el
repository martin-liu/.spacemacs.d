;; packages.el --- m-layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 Martin Liu
;;
;; Author: Martin Liu <hflh1989@gmail.com>
;; URL: https://github.com/martin-liu/m-spacemacs-layer
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq m-layer-packages
      '(
        color-theme-sanityinc-tomorrow
        ace-jump-buffer
        all-the-icons
        ))

;; programmatically define the init functions
(dolist (pkg m-layer-packages)
    (eval `(defun ,(intern (format "m-layer/init-%S" pkg)) nil)))
