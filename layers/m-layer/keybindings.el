;;; basic keybindings
(global-set-key (kbd "M-U") 'move-text-up) ; M + S + u
(global-set-key (kbd "M-D") 'move-text-down)
;; for evil mode
(global-set-key (kbd "M-`") 'evil-escape)

;;; packages' key-bindings
;; ace-jump
(global-set-key (kbd "C-x o") 'ace-window)
(evil-leader/set-key "." 'ace-jump-buffer)

;; helm
(global-set-key (kbd "C-c i") 'helm-imenu)

;;; Hook key-bindings
;; coffee-mode
(add-hook 'coffee-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-r" 'coffee-indent-shift-right)
             (local-set-key "\C-c\C-l" 'coffee-indent-shift-left)
             ))

;; smartparens-mode
(add-hook 'smartparens-mode-hook
          '(lambda ()
             (local-set-key "\C-c\M-w" 'sp-backward-copy-sexp)
             (local-set-key (kbd "C-M-<backspace>") 'sp-backward-kill-sexp)))

;; cider-repl-mode
(add-hook 'cider-repl-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-p" 'cider-repl-previous-input)
             (local-set-key "\C-c\C-n" 'cider-repl-next-input)))


;; martin
(global-set-key (kbd "M-%") 'pcre-query-replace-regexp)
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/GoogleDrive/doc/note/fragments.org")))
(global-set-key (kbd "<f7>") (lambda() (interactive)(find-file "~/GoogleDrive/doc/GTD.org")))

;; `Space o'
(spacemacs/set-leader-keys
  "oo" 'just-one-space
  "oc" 'm-eval-and-append-as-comment
  "oq" 'm-goto-last-change
  "on" 'm-narrow-or-widen-dwim)
