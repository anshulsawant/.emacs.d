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
  "Rename the current buffer and the file it is visiting.
Prompts for a new file name. If the new name is a directory,
the file is moved into that directory with its current name.
Asks for confirmation before overwriting an existing file."
  (interactive "FRename file to: ") ; "F" prompts for a filename
  (let ((old-name (buffer-file-name)))
    ;; 1. Check if the buffer is actually visiting a file.
    (unless old-name
      (error "Buffer is not visiting a file"))

    ;; 2. Determine the final new name. If user enters a directory,
    ;; move the file into it.
    (when (file-directory-p new-name)
      (setq new-name (expand-file-name (file-name-nondirectory old-name) new-name)))

    ;; 3. Check for overwrite, and proceed only if clear.
    (if (or (not (file-exists-p new-name))
            (y-or-n-p (format "File '%s' already exists. Overwrite it? " new-name)))
        (progn
          ;; 4. Rename the file on disk. The '1' means ok-if-already-exists.
          (rename-file old-name new-name 1)
          
          ;; 5. Point the buffer to the new file path.
          ;; The 't' arguments mean "don't ask for confirmation" and "don't mark as modified".
          (set-visited-file-name new-name t t)
          
          ;; 6. Rename the buffer itself to match the new filename.
          ;; The second argument 't' makes the name unique if needed (e.g., "foo.txt<2>").
          (rename-buffer (file-name-nondirectory new-name) t)
          
          ;; 7. Save the buffer to its new location.
          (save-buffer)
          (message "File renamed to '%s'" new-name))
      ;; 8. If the user answered "no" to the overwrite prompt.
      (message "Rename aborted."))))
;; =============================================================================
;; II. CORE EMACS TWEAKS & UI
;; - Basic settings, fonts, theme, and the modeline.
;; =============================================================================

(use-package emacs
  :ensure nil
  :config
  ;; No more loud beeps
  (setq ring-bell-function #'ignore)
  ;; Sensible history and session saving
  (desktop-save-mode 1)
  (savehist-mode 1)
  (recentf-mode 1)
  ;; Font configuration
  (set-face-attribute 'default nil :font "Fira Code Retina" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 120)
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; Make C-x C-c always kill the server
  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs))

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

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
   "bk" '(kill-buffer :wk "Switch Buffer Other Window")
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
   "gg" '(magit-status :wk "Magit Status")
   "gb" '(magit-blame :wk "Magit Blame")
   "gf" '(magit-log-buffer-file :wk "File History Log")
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
  :hook (python-ts-mode . my-dap-python-setup)
  :config
  (require 'dap-ui)
  (dap-ui-mode 1))

(defun my-python-mode-setup ()
  "Function to set up the environment for Python editing."
  (pyvenv-workon (projectile-project-root))
  (eglot-ensure)
  (company-mode)
  (highlight-indent-guides-mode))

;; --- CORRECTED: Keybindings defined individually for robustness ---
(defun my-python-mode-keys ()
  "Sets up the 'SPC m' leader keys specifically for Python mode."
  (general-define-key
   :keymaps 'python-ts-mode-map
   :states 'normal
   :prefix "SPC"

   "m"  '(:ignore t :which-key "Python")
   ;; LSP / Eglot
   "ma" '(eglot-code-actions :wk "Code Actions")
   "md" '(eldoc :wk "Documentation")
   "mf" '(eglot-format-buffer :wk "Format Buffer")
   "mr" '(eglot-rename :wk "Rename Symbol")
   ;; Running Code
   "ms" '(run-python :wk "Run Script")
   ;; Debugging (dap-mode)
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
  :hook ((python-ts-mode . my-python-mode-setup)
         (python-ts-mode . my-python-mode-keys))
  :custom (python-shell-interpreter "ipython3"))

(use-package eglot
  :ensure t
  :straight (:type git :host github :repo "joaotavora/eglot")
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("ruff" "server" "--preview"))))

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

(use-package treesit-auto
  :ensure t
  :straight (:type git :host github :repo "renzmann/treesit-auto")
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :ensure t
  :straight (:type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :hook (treesit-auto-mode . treesit-fold-mode))

(use-package evil-textobj-tree-sitter :ensure t
  :config
  ;; --- THIS IS THE FIX for the query loading ---
  ;; 1. Define our own function that FORCES the use of the richer 'queries' directory.
(defun evil-textobj-tree-sitter--get-queries-dir ()
  "Get the queries directory.
Currently treesit queries are different from queries for elisp-tree-sitter."
  (if (evil-textobj-tree-sitter--use-builtin-treesitter)
      (file-name-as-directory (concat evil-textobj-tree-sitter--dir "treesit-queries"))
    (file-name-as-directory (concat evil-textobj-tree-sitter--dir "queries"))))
  (defun my-force-rich-ts-queries-dir ()
    "Forcibly return the path to the complete `queries` directory."
    (let ((base-dir (file-name-directory (locate-library "evil-textobj-tree-sitter.el"))))
      (expand-file-name "queries" base-dir)))
  
  ;; 2. Tell evil-textobj-tree-sitter to use OUR function instead of its default logic.
  (setq evil-textobj-tree-sitter--get-queries-dir-func #'my-force-rich-ts-queries-dir))

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
  :straight (dotenv :type git
		    :host github
		    :repo "pkulev/dotenv.el"))

(use-package vterm
  :ensure t
  :straight (:type git :host github :repo "akermu/emacs-libvterm"))

(use-package cuda-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package mermaid-mode :ensure t)
