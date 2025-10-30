;; =============================================================================
;; 0. MASTER CONTROL SWITCHES
;; - This new section controls the backend.
;; =============================================================================

(defvar my/use-elisp-tree-sitter t
  "Control which tree-sitter backend to use.
Set to 't' for the feature-rich `elisp-tree-sitter` (requires C compiler).
Set to 'nil' for the built-in native `treesit` (works out-of-the-box).")

;; -- SWITCH: Helper variables derived from the master switch.
(defvar my/python-backend-dependency
  (if my/use-elisp-tree-sitter 'tree-sitter '(treesit-auto treesit-fold))
  "A variable holding the correct dependency for the Python configuration.")

(defvar my/python-major-mode
  (if my/use-elisp-tree-sitter 'python-tree-sitter-mode 'python-ts-mode)
  "The Python major mode to use, based on the chosen backend.")

(defvar my/python-mode-map
  (if my/use-elisp-tree-sitter 'python-tree-sitter-mode-map 'python-ts-mode-map)
  "The keymap for the current Python major mode.")

;; =============================================================================
;; I. BOOTSTRAPPING (straight.el)
;; - This must be first. It sets up the package manager.
;; =============================================================================

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package straight
  :custom (straight-use-package-by-default t))
(use-package project)

(defun my/rename-file-and-buffer (new-name)
  "Rename the current buffer and the file it is visiting."
  (interactive "FRename file to: ")
  (let ((old-name (buffer-file-name)))
    (unless old-name
      (error "Buffer is not visiting a file"))
    (when (file-directory-p new-name)
      (setq new-name (expand-file-name (file-name-nondirectory old-name) new-name)))
    (if (or (not (file-exists-p new-name))
            (y-or-n-p (format "File '%s' already exists. Overwrite it? " new-name)))
        (progn
          (rename-file old-name new-name 1)
          (set-visited-file-name new-name t t)
          (rename-buffer (file-name-nondirectory new-name) t)
          (save-buffer)
          (message "File renamed to '%s'" new-name))
      (message "Rename aborted."))))

;; =============================================================================
;; II. CORE EMACS TWEAKS & UI
;; =============================================================================

;; =============================================================================
;; II. CORE EMACS TWEAKS & UI
;; =============================================================================
;; In your core Emacs tweaks, or anywhere before desktop-save-mode is active.
(use-package emacs
  :ensure nil
  :config
  ;; -- Core Behavior --
  (setq ring-bell-function #'ignore)
  (savehist-mode 1)
  (recentf-mode 1)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

  ;; -- Frame and Window Behavior (for UHD and emacsclient) --
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq-default line-spacing 0.2)
  (setq inhibit-compacting-font-caches t)

  ;; -- Font Configuration (for UHD) --
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 120)
  (set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 120)

  ;; --- CUSTOM CONTEXT-AWARE WINDOW SPLITTING ---
  ;; This redefines the splitting logic to be more predictable.
  ;; It will split side-by-side if the window is wide enough,
  ;; otherwise it will split top-and-bottom.
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)

  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)

  (fmakunbound #'split-window-sensibly)
  (defun split-window-sensibly (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             (split-window window nil (if split-window-right 'left 'right)))
        (and (window-splittable-p window)
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right)))))))

  ;; --- TUNE THE SPLITTING BREAKPOINT HERE ---
  ;; `split-width-threshold`: Minimum width (in columns) for a side-by-side split.
  ;; `split-height-threshold`: Minimum height (in lines) for a top/bottom split.
  (setq-default split-height-threshold 10
                split-width-threshold 120))

(use-package desktop
  :ensure nil ; It's a built-in package
  :hook (after-init . desktop-save-mode) ; A robust way to enable it
  :config
  ;; This is the most semantically correct place for this line.
  (add-to-list 'desktop-modes-not-to-restore 'pdf-view-mode))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")
  :config
  (load-theme 'doom-gruvbox-light t)
  (doom-themes-org-config))
;; In section: II. CORE EMACS TWEAKS & UI, right after doom-themes

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25)))

;; =============================================================================
;; III. THE EDITING EXPERIENCE (Evil, Undo, Keybindings)
;; =============================================================================

(use-package evil
  :ensure t
  :straight (:type git :host github :repo "emacs-evil/evil")
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package general
  :ensure t
  :after evil
  :straight (:type git :host github :repo "noctuid/general.el")
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "a"  '(:ignore t :which-key "AI / Aidermacs")
   "aa" '(aidermacs-transient-menu :wk "Aidermacs Menu")
   "ae" '(my/aidermacs-load-gemini-config :wk "Reload Gemini Config")
   "b"  '(:ignore t :which-key "Buffers")
   "bb" '(consult-buffer :wk "Switch Buffer")
   "bi" '(insert-buffer :wk "Insert Buffer")
   "bk" '(kill-buffer :wk "Kill Buffer")
   "b4b" '(consult-buffer-other-window :wk "Switch Buffer Other Window")
   "f"  '(:ignore t :which-key "Files")
   "ff" '(find-file :wk "Find File")
   "fi" '(insert-file :wk "Find File")
   "f4f" '(find-file-other-window :wk "Find File Other Window")
   "fs" '(save-buffer :wk "Save File")
   "fr" '(my/rename-file-and-buffer :wk "Rename File and Buffer")
   "p"  '(:ignore t :which-key "Projectile")
   "pp" '(projectile-switch-project :wk "Switch Project")
   "pf" '(projectile-find-file :wk "Find File in Project")
   "ps" '(projectile-ripgrep :wk "Search Project (Ripgrep)")
   "g"  '(:ignore t :which-key "Git")
   "gp" '(magit-push :wk "Git Push")
   "gF" '(magit-pull :wk "Git Pull")
   "gc" '(magit-commit :wk "Git Commit")
   "gg" '(magit-status :wk "Magit Status")
   "gb" '(magit-blame :wk "Magit Blame")
   "gf" '(magit-log-buffer-file :wk "File History Log")
   "gs" '(magit-stage-files :wk "Stage File")
   "gd" '(:ignore t :which-key "Diff")
   "gdd" '(magit-ediff-dwim :wk "Dwim")
   "gdc" '(magit-ediff-compare :wk "Compare Revisions")
   "gdu" '(magit-ediff-show-unstaged :wd "Diff Unstaged")
   "gds" '(magit-ediff-show-staged :wk "Diff Staged")
   "s"  '(:ignore t :which-key "Search")
   "so" '(consult-outline :wk "Search Outline")
   "sl" '(consult-line :wk "Search Current Buffer")
   "t"  '(:ignore t :which-key "Toggles")
   "tt" '(consult-load-theme :wk "Choose Theme")
   "w"  '(:ignore t :which-key "Windows")
   "wv" '(evil-window-vsplit :wk "Vertical Split")
   "ws" '(evil-window-split :wk "Horizontal Split")
   "wd" '(delete-window :wk "Delete Window")
   "wh" 'evil-window-left
   "wj" 'evil-window-down
   "wk" 'evil-window-up
   "wl" 'evil-window-right
   "x"  '(:ignore t :which-key "eXecute")
   "xu" '(undo-tree-visualize :wk "Undo Tree Visualize")
   "xe" '(embark-act :wk "Embark Act (Actions)")
   "z"  '(:ignore t :which-key "Fold")
   "zo" '(treesit-fold-open-all :wk "Open All Folds")
   "zc" '(treesit-fold-close-all :wk "Close All Folds")
   "zt" '(treesit-fold-toggle :wk "Toggle Fold at Point")
   "m" '(:ignore t :which-key "Mode Specific"))

  (general-define-key
   :states 'normal
   "Y"  '(consult-yank-pop :wk "Yank from Kill Ring History")
   "gp" '(evil-yank-pop-next :wk "Paste Next from Kill Ring")
   "gP" '(evil-yank-pop-previous :wk "Paste Previous from Kill Ring"))

  ;; This with-eval-after-load block is correctly placed inside the
  ;; :config block of `use-package general`, just as it was in your original file.
  (with-eval-after-load 'evil-textobj-tree-sitter
    (define-key evil-outer-text-objects-map "=" (evil-textobj-tree-sitter-get-textobj "assignment.outer"))
    (define-key evil-inner-text-objects-map "=" (evil-textobj-tree-sitter-get-textobj "assignment.inner"))
    (define-key evil-outer-text-objects-map "<" (evil-textobj-tree-sitter-get-textobj "assignment.lhs"))
    (define-key evil-inner-text-objects-map "<" (evil-textobj-tree-sitter-get-textobj "assignment.lhs"))
    (define-key evil-outer-text-objects-map ">" (evil-textobj-tree-sitter-get-textobj "assignment.rhs"))
    (define-key evil-inner-text-objects-map ">" (evil-textobj-tree-sitter-get-textobj "assignment.rhs"))
    (define-key evil-outer-text-objects-map "P" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
    (define-key evil-inner-text-objects-map "P" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
    (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "call.outer"))
    (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "call.inner"))
    (define-key evil-outer-text-objects-map "S" (evil-textobj-tree-sitter-get-textobj "statement.outer"))
    (define-key evil-inner-text-objects-map "S" (evil-textobj-tree-sitter-get-textobj "statement.outer"))
    (define-key evil-outer-text-objects-map "r" (evil-textobj-tree-sitter-get-textobj "return.outer"))
    (define-key evil-inner-text-objects-map "r" (evil-textobj-tree-sitter-get-textobj "return.inner"))
    (define-key evil-outer-text-objects-map "." (evil-textobj-tree-sitter-get-textobj "@any.outer" '((t . "((*) @any.outer)"))))
    (define-key evil-inner-text-objects-map "." (evil-textobj-tree-sitter-get-textobj "@any.inner" '((t . "((*) @any.inner)"))))

    (general-define-key
     :states 'normal
     "D" #'evil-textobj-tree-sitter-goto-next-node
     "A" #'evil-textobj-tree-sitter-goto-previous-node
     "]f" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer"))
     "[f" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t))
     "]c" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer"))
     "[c" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t)))))

(use-package evil-collection
  :ensure t
  :straight (:type git :host github :repo "emacs-evil/evil-collection")
  :after evil
  :config (evil-collection-init))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (evil-set-undo-system 'undo-tree))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;; =============================================================================
;; IV. COMPLETION FRAMEWORK (Vertico, Consult, Embark)
;; =============================================================================

(use-package vertico
  :ensure t
  :straight (:type git :host github :repo "minad/vertico")
  :init (vertico-mode 1)
  :custom (vertico-cycle t) (vertico-resize nil))

(use-package orderless
  :ensure t
  :straight (:type git :host github :repo "oantolin/orderless")
  :custom (completion-styles '(orderless basic)))

(use-package marginalia
  :ensure t
  :straight (:type git :host github :repo "minad/marginalia")
  :init (marginalia-mode 1))

(use-package consult
  :straight (:type git :host github :repo "minad/consult")
  :ensure t)

(use-package embark
  :ensure t
  :straight (:type git :host github :repo "oantolin/embark")
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult
  :ensure t)

;; =============================================================================
;; V. CORE DEVELOPMENT TOOLS (Git, Projects, Eshell)
;; =============================================================================

(use-package magit
  :straight (:type git :host github :repo "magit/magit")
  :ensure t)

(use-package projectile
  :ensure t
  :straight (:type git :host github :repo "bbatsov/projectile")
  :init (projectile-mode +1)
  :diminish projectile-mode
  :config
  (setq projectile-project-search-path '("~/"))
  (setq projectile-completion-system 'default))

(use-package eshell-git-prompt
  :ensure t
  :after eshell)

(defun configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t)
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell
  :hook (eshell-first-time-mode . configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))

(use-package ediff
  :ensure nil ; It's built-in
  :config
  ;; This explicitly tells the Ediff control frame NOT to be fullscreen,
  ;; overriding the global default-frame-alist setting.
  (add-to-list 'ediff-control-frame-parameters '(fullscreen . nil)))

;; =============================================================================
;; VI. PYTHON & AI DEVELOPMENT ENVIRONMENT
;; =============================================================================

(use-package pyvenv
  :ensure t
  :straight (:type git :host github :repo "jorgenschaefer/pyvenv")
  :config
  (pyvenv-mode 1))

(defun my-dap-python-setup ()
  "Lazily configures dap-mode for Python."
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (dap-register-debug-template
   "My Python Launch"
   (list :type "python"
         :request "launch"
         :name "My Python Launch"
         :program "${file}")))

(use-package dap-mode
  :ensure t
  :straight (:type git :host github :repo "emacs-lsp/dap-mode")
  :hook ((my/python-major-mode . my-dap-python-setup))
  :config
  (require 'dap-ui)
  (dap-ui-mode 1))

(defun my-python-mode-setup ()
  "Function to set up the environment for Python editing."
  (pyvenv-workon (projectile-project-root))
  (eglot-ensure)
  (company-mode)
  (highlight-indent-guides-mode))

(defun my-python-mode-keys ()
  "Sets up the 'SPC m' leader keys specifically for Python mode."
  (general-define-key
   :keymaps my/python-mode-map
   :states 'normal
   :prefix "SPC"
   "m"  '(:ignore t :which-key "Python")
   "ma" '(eglot-code-actions :wk "Code Actions")
   "md" '(eldoc :wk "Documentation")
   "mf" '(eglot-format-buffer :wk "Format Buffer")
   "mr" '(eglot-rename :wk "Rename Symbol")
   "ms" '(run-python :wk "Run Script")
   "mD" '(:ignore t :wk "Debug")
   "mDb" '(dap-breakpoint-toggle :wk "Toggle Breakpoint")
   "mDc" '(dap-continue :wk "Continue")
   "mDd" '(dap-debug :wk "Start Debug Session")
   "mDi" '(dap-step-in :wk "Step In")
   "mDo" '(dap-step-out :wk "Step Out")
   "mDn" '(dap-next :wk "Step Over (Next)")
   "mDq" '(dap-disconnect :wk "Quit Debug Session")
   "mDR" '(dap-ui-repl :wk "Open Debug REPL")))

(use-package python
  :ensure nil ; It's built-in
  :mode (("\\.py\\'" . python-ts-mode))
  ;; -- SWITCH: The hooks are now applied to the dynamic major mode variable.
  :hook ((my/python-major-mode . my-python-mode-setup)
         (my/python-major-mode . my-python-mode-keys))
  :custom (python-shell-interpreter "ipython3"))

(use-package eglot
  :ensure t
  :straight (:type git :host github :repo "joaotavora/eglot")
  :config
  ;; -- SWITCH: Eglot server is associated with the dynamic major mode.
  (add-to-list 'eglot-server-programs
               (list my/python-major-mode . ("ruff" "server" "--preview"))))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package company
  :ensure t
  :straight (:type git :host github :repo "company-mode/company-mode")
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  :config
  (add-to-list 'company-backends 'company-capf))

(use-package conda
  :ensure t
  :straight (:type git :host github :repo "necaris/conda.el")
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3")))

(defun my/aidermacs-configure-for-gemini ()
  "Load ~/.env, find the Gemini key from the returned list, and configure aidermacs."
  (interactive)
  (setq aidermacs-default-model "gemini/gemini-2.5-flash"
        aidermacs-architect-model "gemini/gemini-2.5-pro"
        aidermacs-editor-model "gemini/gemini-2.5-flash")
  (let* ((env-vars (dotenv-load "~/.env"))
         (api-key-pair (assoc "GEMINI_API_KEY" env-vars))
         (api-key-value (and api-key-pair (cadr api-key-pair))))
    (if (and api-key-value (> (length api-key-value) 0))
        (progn
          (setq aidermacs-extra-args `("--api-key" ,(concat "gemini=" api-key-value)
                                      "--chat-language" "en"))
          (message "aidermacs: Successfully configured for Gemini with API key from .env file."))
      (message "aidermacs: Models set for Gemini, but GEMINI_API_KEY not found in ~/.env."))))

(use-package aidermacs
  :ensure t
  :straight (:type git :host github :repo "MatthewZMD/aidermacs")
  :after (evil dotenv projectile)
  :init
  (my/aidermacs-configure-for-gemini)
  :config
  (setq aidermacs-default-chat-mode 'architect
        aidermacs-backend 'vterm
        aidermacs-show-diff-after-change t
        aidermacs-watch-files t
        aidermacs-auto-commits t))

;; =============================================================================
;; VII. UTILITY & MISCELLANEOUS PACKAGES
;; =============================================================================

;; -- SWITCH: This section is now conditional based on the master switch.
(if my/use-elisp-tree-sitter
    ;; If using elisp-tree-sitter, we don't need treesit-auto or treesit-fold
    (progn
      (use-package tree-sitter
        :ensure t
        :straight (:type git :host github :repo "emacs-tree-sitter/elisp-tree-sitter")
        :config
        (require 'tree-sitter-langs)
        (tree-sitter-require 'python)
        (global-tree-sitter-mode)
        :hook (tree-sitter-after-on . tree-sitter-hl-mode))
      (use-package tree-sitter-langs
        :ensure t
        :straight (:type git :host github :repo "emacs-tree-sitter/tree-sitter-langs")
        :after tree-sitter))
  ;; If using native treesit, load your original packages
  (progn
    (use-package treesit-auto
      :ensure t
      :straight (:type git :host github :repo "renzmann/treesit-auto")
      :config
      (treesit-auto-add-to-auto-mode-alist 'all)
      (global-treesit-auto-mode))

    (use-package treesit-fold
      :ensure t
      :straight (:type git :host github :repo "emacs-tree-sitter/treesit-fold")
      :hook (treesit-auto-mode . treesit-fold-mode))))

;; -- SWITCH: The original evil-textobj-tree-sitter declaration has been modified
;; to remove the hack and correctly wait for the backend to load.
(use-package evil-textobj-tree-sitter
  :ensure t
  :after (evil-collection tree-sitter treesit-auto) ; Waits for whichever backend is loaded
  :config
  ;; The hacky functions from your original config are now correctly removed.
  ;; The package will correctly auto-detect the backend.
  )

(use-package highlight-indent-guides
  :ensure t
  :straight (:type git :host github :repo "DarthFennec/highlight-indent-guides")
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face "gray25"))

(use-package rainbow-delimiters
  :ensure t
  :straight (:type git :host github :repo "Fanael/rainbow-delimiters")
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dotenv
  :ensure t
  :straight (:type git
             :host github
             :repo "pkulev/dotenv.el"))

(use-package vterm
  :ensure t
  :straight (:type git :host github :repo "akermu/emacs-libvterm"))

;; (use-package pdf-tools
;;   :ensure t
;;   :straight (:type git :host github :repo "vedang/pdf-tools")
;;   ;; We remove EVERYTHING else. No :defer, no :config, no :magic.
;;   ;; This makes the block inert at startup.
;; )
;; (defun my/configure-pdf-tools-after-frame ()
;;   "Configure pdf-tools once a graphical frame is available.
;; This function is intended to be run from `server-after-make-frame-hook`."
;;   ;; Now we do the setup that was failing before.
;;   ;; The require call will load the package.
;;   (require 'pdf-tools)

;;   ;; This is the standard way to associate PDFs with pdf-view-mode.
;;   ;; It's an autoload, so it's efficient.
;;   (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;;   ;; Install the server if it's not already built.
;;   (pdf-tools-install :no-query)

;;   ;; Add your custom hooks.
;;   (add-hook 'pdf-view-mode-hook #'pdf-view-fit-width-to-window))

;; ;; This is the crucial part. It tells the Emacs server to run our
;; ;; setup function, but only after a client has connected.
;; (add-hook 'server-after-make-frame-hook #'my/configure-pdf-tools-after-frame)


(use-package cuda-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package mermaid-mode :ensure t)
(put 'narrow-to-region 'disabled nil)
