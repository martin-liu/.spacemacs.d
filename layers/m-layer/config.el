;; Windows: Use windows key as Super key
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key
;; MAC: Use command key as M, option key as S
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(global-visual-line-mode t)
(global-linum-mode t)

;; debug on error
(setq debug-on-error t)

;; disable lockfiles
(setq create-lockfiles nil)
;; save buffer when focus out
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; delete trailing whitespace when save file
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; ace-window, used to jump between windows
(setq aw-keys '(?d ?h ?t ?n ?s ?6 ?7 ?8 ?9))
;; ace-jump-buffer
(setq avy-keys (number-sequence ?a ?z))

;; remote file
(setq tramp-default-method "ssh")

;; Org mode

(eval-after-load 'org-mode
  '(add-hook 'org-mode-hook
           (lambda ()
             (setq truncate-lines nil)
             (setq org-startup-indented t)
             (auto-fill-mode 1)
             ;; highlight code
             (setq org-src-fontify-natively t)
             (org-display-inline-images))))

;;; Set default org file to store note
(setq org-default-notes-file "~/Dropbox/Martin/doc/GTD.org")
(setq org-agenda-files '("~/Dropbox/Martin/doc/"))

;;; Set org tag list
(setq org-tag-alist '(("@work" . ?w) ("@me" . ?m)))

;;; Quickly use C-c C to take note
(global-set-key (kbd "C-c C") 'org-capture)
;;; Setup capture template
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "" "Tasks") "* TODO %?\n %i\n")
        ("n" "NOTE" entry (file+headline "" "Notes") "* NOTE - %?\n %i\n %a")
        ("j" "Journal" entry (file+datetree "~/Dropbox/Martin/doc/journal.org")
         "* %U\n%?")))

;; Org export
(setq org-export-htmlize-output-type 'css)


;;; Styles
;; all-the-icons
(setq inhibit-compacting-font-caches t)
;; neotree use all-the-icons theme
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; -------------------------------------------
;;; Programming
;; -------------------------------------------

(setq projectile-use-git-grep 't)

;; setup indent
(m-setup-indent 2)


;;; Python
(setq python-shell--interpreter 'python)
(setq python-shell--interpreter-args "-i")
(setq python-shell-prompt-detect-failure-warning nil)

;;; Scheme
;; geiser
(setq geiser-active-implementations '(guile))

;;; Scala
;; ensime
(setq ensime-sbt-command "/usr/local/bin/sbt")
;(define-key company-active-map [tab] nil)


;;; Clojure
;; clojurescript
(setq cider-cljs-lein-repl
      "(do (user/run)
           (user/browser-repl))")

;;; Web
;; web-mode
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))


;;(setq url-proxy-services
;;      '(("no_proxy" . "^\\(localhost\\|127.0.0.1\\)")
;;        ("http" . "127.0.0.1:8118")))


;; sh
;(spacemacs|defvar-company-backends sh-mode)
;(spacemacs|add-company-hook sh-mode)
