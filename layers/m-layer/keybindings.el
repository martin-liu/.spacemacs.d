;;; basic keybindings
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;;; packages' key-bindings
(global-set-key (kbd "C-x o") 'ace-window)
(evil-leader/set-key "." 'ace-jump-buffer)

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
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/Dropbox/Martin/doc/note/fragments.org")))
(global-set-key (kbd "<f7>") (lambda() (interactive)(find-file "~/Dropbox/Martin/doc/GTD.org")))
