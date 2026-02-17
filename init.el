(server-start)

;;; Useful behaviors ----

(setq inhibit-startup-screen t)     ; Disable startup screen
(setq initial-scratch-message "")   ; Make *scratch* buffer blank
(setq ring-bell-function 'ignore)   ; Disable bell sound
(fset 'yes-or-no-p 'y-or-n-p)       ; y-or-n-p makes answering questions faster
(show-paren-mode 1)                 ; Show closing parens by default
(delete-selection-mode 1)           ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t)         ; Auto-update buffer if file has changed on disk
(setq scroll-conservatively 101)    ; Vim-like scrolling

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Move faster between windows using the shift and arrow keys
(windmove-default-keybindings)
;; Window layout history (C-c <right>, C-c <left>)
(winner-mode 1)

;; Saves the recently visited files across emacs sessions
(recentf-mode 1)

;;; Lines ----

(setq-default truncate-lines nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default fill-column 80)
(setq line-move-visual t) ;; C-p, C-n, etc uses visual lines

;;;; Fonts ----

(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(set-face-attribute 'fixed-pitch nil
                    :family "Fira Code"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Code"
                    ;; :family "Open Sans"
                    :height 150
                    :weight 'normal
                    :width 'normal)

;;;; Agenda and calendar ---

(setq calendar-date-style 'european     ; day/month/year
      calendar-week-start-day 1         ; start on monday
      calendar-day-header-array ["Do" "Lu" "Ma" "Mi" "Ju" "Vi" "Sá"]
      calendar-day-name-array ["domingo" "lunes" "martes" "miércoles" "jueves" "viernes" "sábado"]
      calendar-month-abbrev-array ["Ene" "Feb" "Mar" "Abr" "May" "Jun" "Jul" "Ago" "Sep" "Oct" "Nov" "Dic"]
      calendar-month-name-array ["enero" "febrero" "marzo" "abril" "mayo" "junio" "julio" "agosto" "septiembre" "octubre" "noviembre" "diciembre"])

;;;; Packages ----

(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package tab-bar
  :bind (("C-<right>" . tab-next)
		 ("C-<left>" . tab-previous)))

;; (use-package ef-themes
;;   :init
;;   ;; This makes the Modus commands listed below consider only the Ef
;;   ;; themes.  For an alternative that includes Modus and all
;;   ;; derivative themes (like Ef), enable the
;;   ;; `modus-themes-include-derivatives-mode' instead.  The manual of
;;   ;; the Ef themes has a section that explains all the possibilities:
;;   ;;
;;   ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
;;   ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
;;   (ef-themes-take-over-modus-themes-mode 1)
;;   :bind
;;   (("<f5>" . modus-themes-rotate)
;;    ("C-<f5>" . modus-themes-select)
;;    ("M-<f5>" . modus-themes-load-random))
;;   :config
;;   ;; All customisations here.
;;   (setq modus-themes-mixed-fonts t)
;;   (setq modus-themes-italic-constructs t)

;;   ;; Finally, load your theme of choice (or a random one with
;;   ;; `modus-themes-load-random', `modus-themes-load-random-dark',
;;   ;; `modus-themes-load-random-light').
;;   ;; (modus-themes-load-theme 'ef-elea-dark)
;;   (modus-themes-load-theme 'ef-owl)
;;   )

;;; For packaged versions which must use `require'.
(use-package modus-themes
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme of your choice.
  (modus-themes-load-theme 'modus-vivendi)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; Hide minor modes
(use-package minions
  :config
  (minions-mode))

(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  :hook (prog-mode . tree-sitter-hl-mode))
(use-package tree-sitter-langs)

(use-package undo-tree  ; Enable undo-tree, sane undo/redo behavior
  :init (global-undo-tree-mode)
  :config
  (add-hook 'before-save-hook
      'delete-trailing-whitespace)    ; Delete trailing whitespace on save
  (setq undo-tree-history-directory-alist
        '(("." . "/tmp/emacs-undo-tree")))) ; prevent undo-tree to pollute current dir

(use-package exec-path-from-shell
  :init (when (display-graphic-p)
		  (exec-path-from-shell-initialize)))

;; (use-package evil
;;   :init (evil-mode 1))
;; (use-package evil-collection
;;   :init (evil-collection-init))

(use-package crux
  :bind (
		 ("C-k" . crux-smart-kill-line)
		 ("C-K" . crux-kill-whole-line)
		 ("M-RET" . crux-smart-open-line-above)
		 ("C-c f" . crux-recentf-find-file)
		 ("C-c D" . crux-delete-current-file-and-buffer)
		 ("C-c d" . crux-duplicate-current-line-or-region)
		 ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
		 ("C-c r" . crux-rename-file-and-buffer)
		 ("C-c t" . crux-visit-term-buffer) ; visit ansi-term
		 ("C-c k" . crux-kill-other-buffers) ; Kill all open buffers except the one you're currently in
		 ("C-c I" . crux-find-user-init-file) ; open init.el
		 ("C-c J" . crux-top-join-line) ; Same as Vim's Shif-J
		 ))

(use-package which-key
  :config
  (which-key-mode))

(use-package vertico
  :init
  (vertico-mode))

;; Richer annotations in the minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Persist history over Emacs restarts.
;; Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package magit
  :defer t)

;;; Completion ----

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  :init
  ;; Enable Corfu globally.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  ;; (corfu-popupinfo-mode)

  :config
  ;; Enable auto completion, configure delay, trigger and quitting
  (setq corfu-auto t
		corfu-auto-delay 0.2
		corfu-auto-trigger "." ;; Custom trigger characters
		corfu-quit-no-match 'separator) ;; or t
  )

(use-package orderless
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  ;; (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
 )

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-emoji)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file))


;;; LSP and errors ----
(use-package eglot
  :hook
  ((c-mode . eglot-ensure)
   (rustic-mode . eglot-ensure)
   (python-mode . eglot-ensure))
   (LaTeX-mode . eglot-ensure))
 ;;  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
 ;;  (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
 ;;  (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
 ;; )

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;;; Languages ----
(use-package tex
  :straight auctex
  :config
  (setq TeX-engine-alist '((default
                            "Tectonic"
                            "tectonic -X compile -f plain %T"
                            "tectonic -X watch"
                            nil))
        LaTeX-command-style '(("" "%(latex)"))
        TeX-process-asynchronous t
        TeX-check-TeX nil
        TeX-engine 'default)

  (let ((tex-list   (assoc "TeX"   TeX-command-list))
        (latex-list (assoc "LaTeX" TeX-command-list)))
    (setf (cadr tex-list)   "%(tex)"
          (cadr latex-list) "%l")))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package org
  :bind ("C-c a" . org-agenda))
(use-package org-modern
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Fira Code")
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0
   org-modern-star '("●" "○" "✸" "✿")
   org-ellipsis "…")
  (global-org-modern-mode))
