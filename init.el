;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(rust
     restclient
     nginx
     octave
     csv
     ;ansible
     yaml
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     org
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-term-shell "/bin/zsh")
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     (spell-checking :variables spell-checking-enable-by-default t)
     version-control
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          ;; This is really creepy magit
          magit-revision-show-gravatars nil)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)

     github

     lsp
     ;; languages
     markdown
     latex
     sql
     emacs-lisp
     ;scheme
     ;lua
     ;haskell
     ;java ;; disable java layer, use scala layer to support java
     scala
     kotlin
     (clojure :variables clojure-enable-fancify-symbols t)
     (python :variables python-backend 'lsp)
     ;php
     html
     javascript
     typescript
     react
     (go :variables go-backend 'lsp)
     ;rust

     docker
     protobuf

     ;; my layer
     m-layer
     )
   rust-enable-racer t
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-mode-line-theme 'spacemacs
   dotspacemacs-themes '(sanityinc-tomorrow-eighties
                         leuven
                         monokai
                         zenburn
                         )
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("M+ 1m"
                               :size 22
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'nil
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   dotspacemacs-line-numbers t
   dotspacemacs-enable-server t
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  ;; use paredit bindings
  (sp-use-paredit-bindings)
  (global-company-mode)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("/Users/hualiu/GoogleDrive/doc/GTD.org" "/Users/hualiu/GoogleDrive/doc/journal.org")))
 '(package-selected-packages
   (quote
    (go-dlv yasnippet-snippets all-the-icons memoize restclient-helm ob-restclient ob-http company-restclient restclient know-your-http-well nginx-mode csv-mode gntp parent-mode gitignore-mode fringe-helper git-gutter+ git-gutter marshal logito ht pos-tip flx anzu goto-chg json-snatcher json-reformat web-completion-data dash-functional peg eval-sexp-fu spinner queue pkg-info epl bind-map pythonic popup undo-tree ghc gh seq s diminish f jinja2-mode company-ansible ansible-doc ansible log4e winum unfill fuzzy typescript-mode multiple-cursors async anaconda-mode flyspell-correct packed sbt-mode evil avy dash auto-complete simple-httpd yaml-mode org skewer-mode scala-mode rust-mode tern iedit go-mode request pcache alert hydra haml-mode bind-key smartparens highlight flycheck which-key web-mode spaceline smeargle racer php-extras persp-mode org-plus-contrib open-junk-file neotree markdown-toc live-py-mode info+ htmlize helm-projectile evil-mc drupal-mode docker company-statistics clj-refactor clojure-mode helm helm-core haskell-mode company projectile magit magit-popup git-commit yasnippet php-mode js2-mode yapfify xterm-color ws-butler with-editor window-numbering web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tide tagedit tablist sql-indent spacemacs-theme slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort pug-mode powerline popwin pip-requirements phpunit phpcbf php-auto-yasnippets pcre2el paredit paradox orgit org-projectile org-present org-pomodoro org-download org-bullets noflet mwim multi-term move-text mmm-mode markdown-mode magit-gitflow magit-gh-pulls macrostep lua-mode lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc intero inflections indent-guide ido-vertical-mode ibuffer-projectile hy-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio go-guru go-eldoc gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md geiser flyspell-correct-helm flycheck-rust flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help ensime emmet-mode elisp-slime-nav edn dumb-jump dockerfile-mode docker-tramp diff-hl define-word cython-mode company-web company-tern company-go company-ghci company-ghc company-cabal company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-identifiers-mode coffee-mode cmm-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu cider cargo auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ace-jump-buffer ac-ispell)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("/Users/hualiu/GoogleDrive/doc/GTD.org" "/Users/hualiu/GoogleDrive/doc/journal.org")))
 '(package-selected-packages
   (quote
    (lsp-rust helm-gtags ggtags counsel-gtags go-dlv yasnippet-snippets all-the-icons memoize restclient-helm ob-restclient ob-http company-restclient restclient know-your-http-well nginx-mode csv-mode gntp parent-mode gitignore-mode fringe-helper git-gutter+ git-gutter marshal logito ht pos-tip flx anzu goto-chg json-snatcher json-reformat web-completion-data dash-functional peg eval-sexp-fu spinner queue pkg-info epl bind-map pythonic popup undo-tree ghc gh seq s diminish f jinja2-mode company-ansible ansible-doc ansible log4e winum unfill fuzzy typescript-mode multiple-cursors async anaconda-mode flyspell-correct packed sbt-mode evil avy dash auto-complete simple-httpd yaml-mode org skewer-mode scala-mode rust-mode tern iedit go-mode request pcache alert hydra haml-mode bind-key smartparens highlight flycheck which-key web-mode spaceline smeargle racer php-extras persp-mode org-plus-contrib open-junk-file neotree markdown-toc live-py-mode info+ htmlize helm-projectile evil-mc drupal-mode docker company-statistics clj-refactor clojure-mode helm helm-core haskell-mode company projectile magit magit-popup git-commit yasnippet php-mode js2-mode yapfify xterm-color ws-butler with-editor window-numbering web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tide tagedit tablist sql-indent spacemacs-theme slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort pug-mode powerline popwin pip-requirements phpunit phpcbf php-auto-yasnippets pcre2el paredit paradox orgit org-projectile org-present org-pomodoro org-download org-bullets noflet mwim multi-term move-text mmm-mode markdown-mode magit-gitflow magit-gh-pulls macrostep lua-mode lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc intero inflections indent-guide ido-vertical-mode ibuffer-projectile hy-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio go-guru go-eldoc gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md geiser flyspell-correct-helm flycheck-rust flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eshell-z eshell-prompt-extras esh-help ensime emmet-mode elisp-slime-nav edn dumb-jump dockerfile-mode docker-tramp diff-hl define-word cython-mode company-web company-tern company-go company-ghci company-ghc company-cabal company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-identifiers-mode coffee-mode cmm-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu cider cargo auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ace-jump-buffer ac-ispell)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
)
