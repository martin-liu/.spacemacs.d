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
        exec-path-from-shell
        color-theme-sanityinc-tomorrow
        ace-jump-buffer
        yasnippet-snippets
        all-the-icons
        go-dlv                          ; go debug tool `delve'
        ;; https://github.com/yjwen/org-reveal
        ox-reveal
        ))

;; programmatically define the init functions
(dolist (pkg m-layer-packages)
    (eval `(defun ,(intern (format "m-layer/init-%S" pkg)) nil)))

(defun m-layer/init-exec-path-from-shell()
  (use-package exec-path-from-shell
    :init (when (or (spacemacs/system-is-mac)
                    (spacemacs/system-is-linux)
                    (memq window-system '(x)))
            (exec-path-from-shell-initialize))))

(defun m-layer/init-ox-reveal ()
  (use-package ox-reveal
    :config
    (progn
      (load-library "ox-reveal")
      )
    )
  )
