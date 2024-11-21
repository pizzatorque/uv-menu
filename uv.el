(require 'transient)
(require 'project)

(defvar uv-shell-command "uv --color=never")

;; include uv run
;; integrate with embark

(defun uv-init-prefix-init (obj)
  "Default prefix path for the init command.
OBJ is required by prefix."
  (oset obj value `(,(format "--path=%s" (if
					     buffer-file-name
					     (file-name-directory buffer-file-name)
					   default-directory)))))


;; refactor this
(defun uv-add-prefix-init (obj)
  "Default prefix for the add command.
OBJ is required by prefix."
  ;;; refactor this
  (let* ((req-file (cond ((file-exists-p
			 (concat (file-name-directory buffer-file-name)
				 "requirements.txt"))
			 (concat (file-name-directory buffer-file-name)
				 "requirements.txt"))
			((file-exists-p
			  (concat default-directory "requirements.txt"))
			 (concat default-directory "requirements.txt"))
			((and (project-current) (file-exists-p (concat (project-root (project-current)) "requirements.txt")))
			 (concat (project-root (project-current)) "requirements.txt"))
			(t nil)))
	 (pkg (when (and (symbol-at-point) (not req-file))
	       t)))
    (oset obj value `(,(when req-file (concat (format "--requirements=%s" req-file)))
		      ,(if pkg
			   (format "--package=%s" (symbol-at-point))
			 nil)))))


(defun uv-venv-prefix-init (obj)
  "Default prefix for the venv command path.
OBJ is required by prefix."
  (oset obj value `(,(concat (format "--path=%s" (if
					     buffer-file-name
					     (file-name-directory buffer-file-name)
					   default-directory)) ".venv"))))

(transient-define-argument uv-path--path ()
  "Environment Path Argument."
  :class 'transient-option
  :shortarg "pt"
  :description "Environment root path"
  :always-read t
  :argument "--path="
  :reader (lambda (_prompt _initial _history) (read-file-name _prompt)))

(transient-define-argument uv--refresh ()
  "Refresh the cache."
  :shortarg "rf"
  :description "Refresh all cached data"
  :argument "--refresh")

(transient-define-argument uv--no-cache ()
  "No Cache Argument."
  :shortarg "nc"
  :description "Avoid reading from or writing to the cache, instead using a temporary
directory for the duration of the operation."
  :argument "--no-cache")

(defun uv-parse-arg-flag (flag args)
  "Return FLAG and arg from ARGS concatenated."
  (pcase (transient-arg-value flag args)
    ((and (pred booleanp) value)
     (if value
	 flag
       ""))
    ((and (pred stringp) value)
     (let ((concatenated-flag (concat flag value))) ; change to pcase
       (pcase concatenated-flag
	   ((guard (and (string= (substring flag -1) "=")
			(string= (substring concatenated-flag -1) "=")))
	    "")
	   ((guard (string= concatenated-flag flag))
	    concatenated-flag)
	   ((guard (string= flag "--path="))
	    (substring concatenated-flag 7))
	   ((guard (string= flag "--package="))
	    (substring concatenated-flag 10))
	   (_
	    concatenated-flag))))))

(defun uv--gen-format-string (n)
  "Build string with N number of arguments to be formatted."
  (let ((unit "%s "))
    (dotimes (_ n) (setq unit (concat "%s " unit)))
    unit))

(defun uv--execute-command (command uv-python-args uv--buffer-name)
  "Use given COMMAND followed by UV-PYTHON-ARGS.
Start execution in given UV--BUFFER-NAME."
  (let* ((pr (if (project-current)
		(project-root (project-current))
	      nil))
	(default-directory (or pr (file-name-directory buffer-file-name))))
    (compilation-start (concat uv-shell-command " " command " " uv-python-args)
		       nil
		       (lambda (_) uv--buffer-name))))

(transient-define-suffix uv-init-command (&optional args)
  :key "t"
  :description "UV init command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-init-args-str (uv--gen-format-string 5))
	 (uv-proj-path (uv-parse-arg-flag "--path=" args))
	 (uv-proj-name (uv-parse-arg-flag "--name=" args))
	 (uv-package (uv-parse-arg-flag "--package" args))
	 (uv-no-readme (uv-parse-arg-flag "--no-readme" args))
	 (uv-python-version (uv-parse-arg-flag "--python=" args))
	 (uv-config-file-path (uv-parse-arg-flag "--config-file=" args))
	 (uv-init-args (format uv-init-args-str
			       uv-proj-name
			       uv-package
			       uv-no-readme
			       uv-python-version
			       uv-config-file-path
			       uv-proj-path)))
    (uv--execute-command "init" uv-init-args (format "UV INIT %s" uv-proj-path))))

(transient-define-suffix uv-lock-command (&optional args)
  :key "k"
  :description "UV lock command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-lock-args-str (uv--gen-format-string 6))
	 (uv-locked (uv-parse-arg-flag "--locked"   args))
	 (uv-frozen (uv-parse-arg-flag "--frozen"   args))
	 (uv-no-cache (uv-parse-arg-flag   "--no-cache" args))
	 (uv-refresh (uv-parse-arg-flag "--refresh"  args))
	 (uv-python-version (uv-parse-arg-flag "--python=" args))
	 (uv-upgrade (uv-parse-arg-flag "--upgrade" args))
	 (uv-no-sources (uv-parse-arg-flag "--no-sources" args))
	 (uv-lock-args (format uv-lock-args-str
			       uv-python-version
			       uv-locked
			       uv-frozen
			       uv-no-cache
			       uv-upgrade
			       uv-no-sources
			       uv-refresh)))
    (uv--execute-command "lock" uv-lock-args "UV LOCK")))

(transient-define-suffix uv-python-install-command (&optional args)
  :key "i"
  :description "UV python install command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-python-args-str (uv--gen-format-string 2))
	 (uv-no-cache (uv-parse-arg-flag   "--no-cache" args))
	 (uv-refresh (uv-parse-arg-flag "--refresh"  args))
	 (uv-python-version (uv--read-python-version-choice))
	 (uv-python-args (format uv-python-args-str
			       uv-no-cache
			       uv-refresh
			       uv-python-version)))
    (uv--execute-command "python install" uv-python-args "UV PYTHON INSTALL")))

(transient-define-prefix uv-lock-menu ()
  "UV lock transient interface."
  ["Options"
   (uv-locked-choice)
   (uv-frozen-choice)
   ]
  ["Resolver Options"
   ("u" "" "--upgrade")
   ("ns" "" "--no-sources")]
  [["Cache Options"
   (uv--no-cache)
   (uv--refresh)]
  ["UV Python Options"
   (uv-python-choice)]]
  [["UV lock command"
    (uv-lock-command)]])

(transient-define-prefix uv-python-menu ()
  "UV python transient interface."
  ["Cache Options"
   (uv--no-cache)
   (uv--refresh)]
  [["UV python command"
    (uv-python-install-command)]])

(defun uv--get-available-pip-packages ()
  "Return the list of all installed pip packages."
  (let ((content (shell-command-to-string "uv pip list"))
	(pos 0)
	matches)
    (while (string-match "^[A-Za-z\-\_0-9]+" content pos)
      (unless (or (string= (match-string 0 content) "Package")
		  (string= "---------------" (match-string 0 content)))
	(push (match-string 0 content) matches))
      (setq pos (match-end 0)))
    matches))

(defun uv--get-available-python-versions ()
  "Return the list of all available python versions."
  (let ((content (shell-command-to-string "uv python list"))
	(pos 0)
	matches)
    (while (string-match "[0-9]+.[0-9]+.[0-9]+" content pos)
      (push (match-string 0 content) matches)
      (setq pos (match-end 0)))
    matches))

(defun uv--parse-python-version (version)
  "Return the given python VERSION without periods."
  `(,version . ,(replace-regexp-in-string "\\." "" version)))

(transient-define-suffix uv-add-command (&optional args)
  :key "a"
  :description "UV add command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-add-args-str (uv--gen-format-string 15))
	 (uv-pa (uv-parse-arg-flag "--package=" args))
	 (uv-locked (uv-parse-arg-flag "--no-project" args))
	 (uv-frozen (uv-parse-arg-flag "--seed" args))
	 (uv-dev (uv-parse-arg-flag "--dev" args))
	 (uv-optional (uv-parse-arg-flag "--optional=" args))
	 (uv-editable (uv-parse-arg-flag "--editable" args))
	 (uv-no-sync (uv-parse-arg-flag "--no-sync" args))
	 (uv-upgrade (uv-parse-arg-flag "--upgrade" args))
	 (uv-upgrade-package (uv-parse-arg-flag "--upgrade-package=" args))
	 (uv-reinstall (uv-parse-arg-flag "--reinstall" args))
	 (uv-reinstall-package (uv-parse-arg-flag "--reinstall-package=" args))
	 (uv-no-cache (uv-parse-arg-flag "--no-cache" args))
	 (uv-refresh (uv-parse-arg-flag "--refresh" args))
	 (uv-index-strategy (uv-parse-arg-flag "--index-strategy=" args))
	 (uv-keyring-provider (uv-parse-arg-flag "--keyring-provider" args))
	 (uv-python-choice (uv-parse-arg-flag "--python=" args))
	 (uv-add-args (format
			uv-pa
			uv-locked
			uv-frozen
			uv-dev
			uv-optional
			uv-editable
			uv-no-sync
			uv-upgrade
			uv-upgrade-package
			uv-reinstall
			uv-reinstall-package
			uv-no-cache
			uv-refresh
			uv-keyring-provider
			uv-python-choice)))
    (uv--execute-command "add"  uv-add-args  "UV ADD")))

(transient-define-suffix uv-venv-command (&optional args)
  :key "v"
  :description "UV venv command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-venv-args-str (uv--gen-format-string 9))
	 (uv-proj-path (uv-parse-arg-flag "--path=" args))
	 (uv-no-proj (uv-parse-arg-flag "--no-project" args))
	 (uv-seed (uv-parse-arg-flag "--seed" args))
	 (uv-relocatable (uv-parse-arg-flag "--relocatable" args))
	 (uv-system-site-pkg (uv-parse-arg-flag "--system-site-packages" args))
	 (uv-index-strategy (uv-parse-arg-flag "--system-strategy=" args))
	 (uv-link-mode (uv-parse-arg-flag "--link-mode" args))
	 (uv-allow-existing (uv-parse-arg-flag "--allow-existing" args))
	 (uv-keyring-provider (uv-parse-arg-flag "--keyring-provider" args))
	 (uv-python-version (uv-parse-arg-flag "--python=" args))
	 (uv-venv-args (format uv-venv-args-str
			       uv-relocatable
			       uv-system-site-pkg
			       uv-index-strategy
			       uv-link-mode
			       uv-no-proj
			       uv-python-version
			       uv-seed
			       uv-allow-existing
			       uv-keyring-provider
			       uv-proj-path)))
    (uv--execute-command "venv"  uv-venv-args (format "UV VENV %s" uv-proj-path))))

(transient-define-suffix uv-pip-list-command (&optional args)
  :key "l"
  :description "UV pip list command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-pip-list-args-str (uv--gen-format-string 2))
	 (uv-editable (uv-parse-arg-flag "--editable" args))
	 (uv-strict (uv-parse-arg-flag "--strict" args))
	 (uv-system (uv-parse-arg-flag "--system" args))
	 (uv-cache-option (uv-parse-arg-flag "--no-cache" args))
	 (uv-python-version (uv-parse-arg-flag "--python=" args))
	 (uv-venv-args (format uv-pip-list-args-str
			       uv-editable
			       uv-strict
			       uv-system
			       uv-cache-option
			       uv-python-version)))
    (uv--execute-command "pip list"  uv-venv-args "UV PIP LIST")))

(transient-define-suffix uv-pip-show-command (&optional args)
  :key "x"
  :description "UV pip show command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-pip-list-args-str (uv--gen-format-string 4))
	 (uv-show-package (uv-parse-arg-flag "--package=" args))
	 (uv-strict (uv-parse-arg-flag "--strict" args))
	 (uv-system (uv-parse-arg-flag "--system" args))
	 (uv-cache-option (uv-parse-arg-flag "--no-cache" args))
	 (uv-python-version (uv-parse-arg-flag "--python=" args))
	 (uv-venv-args (format uv-pip-list-args-str
			       uv-show-package
			       uv-strict
			       uv-system
			       uv-cache-option
			       uv-python-version)))
    (uv--execute-command "pip show"  uv-venv-args "UV PIP SHOW ")))

(defun uv--read-pip-show-package ()
  "Prompt for a python pip package to choose."
  (let ((_ '(:annotation-function uv-python-set)))
	(completing-read "Choose: " (uv--get-available-pip-packages))))

(defun uv--read-python-version-choice ()
  "Prompt for a python version to choose."
  (let ((_ '(:annotation-function uv-python-set)))
	(completing-read "Choose: " (uv--get-available-python-versions))))

(transient-define-argument uv-python-choice ()
  "Execution Path Argument."
  :class 'transient-option
  :shortarg "py"
  :description "The Python interpreter to use for the project environment."
  :argument "--python="
  :reader (lambda (_prompt _initial _history) (uv--read-python-version-choice)))

(transient-define-argument uv-requirements-choice ()
  "Requirements File Path Argument."
  :class 'transient-option
  :shortarg "-r"
  :description "Add all packages listed in the given requirements.txt files."
  :argument "--requirements="
  :reader (lambda (_prompt _initial _history) (read-file-name _prompt)))


(defun uv--read-link-mode-choice ()
  "Prompt for a python version to choose."
  (completing-read "Choose: " '("clone" "copy" "hardlink" "symlink")))

(transient-define-argument uv-link-mode-choice ()
  "Requirements File Path Argument."
  :class 'transient-option
  :shortarg "lm"
  :description "The method to use when installing packages from the global cache"
  :argument "--link-mode="
  :reader (lambda (_prompt _initial _history) (uv--read-link-mode-choice)))

(defun uv--read-index-strategy-choice ()
  "Prompt for a python version to choose."
  (completing-read "Choose: " '("first-index" "unsafe-first-match" "unsafe-best-match")))

(defun uv--read-keyring-provider-choice ()
  "Prompt for a python version to choose."
  (completing-read "Choose: " '("disabled" "subprocess")))

(transient-define-argument uv-index-strategy-choice ()
  :class 'transient-option
  :shortarg "is"
  :description "The strategy to use when resolving against multiple index URLs"
  :argument "--index-strategy="
  :reader (lambda (_prompt _initial _history) (uv--read-index-strategy-choice)))

(transient-define-argument uv--pip-show-package ()
  :class 'transient-option
  :shortarg "sp"
  :description "Display an installed package"
  :argument "--package="
  :reader (lambda (_prompt _initial _history) (uv--read-pip-show-package)))

(transient-define-argument uv-keyring-provider-choice ()
  :class 'transient-option
  :shortarg "kr"
  :description "Attempt to use `keyring` for authentication for index URLs"
  :argument "--keyring-provider="
  :reader (lambda (_prompt _initial _history) (uv--read-keyring-provider-choice)))

(transient-define-prefix uv-venv-menu ()
  :init-value 'uv-venv-prefix-init
  ["Options"
   (uv-path--path)
   ("n" "Do not discover a project or workspace" "--no-project")
   ("se" "Install seed packages (one or more of: `pip`, `setuptools`, and `wheel`)
into the virtual environment" "--seed")
   ("a" "Preserve any existing files or directories at the target path" "--allow-existing")
   ("sy" "Give the virtual environment access to the system site packages
directory" "--system-site-packages")
   ("re" "Make the virtual environment relocatable" "--relocatable")
   (uv-index-strategy-choice)
   (uv-link-mode-choice)
   (uv-keyring-provider-choice)]
  ["UV Python Options"
   (uv-python-choice)]
  [["UV venv command"
    (uv-venv-command)]])

(transient-define-prefix uv-add-menu ()
  "UV init transient interface."
  :init-value 'uv-add-prefix-init
  ["Arguments"
   ("pa" "" "--package=")]
  ["Options"
   (uv-requirements-choice)
   (uv-locked-choice)
   (uv-frozen-choice)
   ("d" "" "--dev")
   ("o" "" "--optional=")
   ("e" "" "--editable")
   ("ns" "" "--no-sync")]
  [["Index Options"
   (uv-index-strategy-choice)
   (uv-keyring-provider-choice)]]
  [["Resolver options"
  ("upg" "" "--upgrade")
  ("upk" "" "--upgrade-package=")]
   ["Installer options"
   ("ra" "" "--reinstall")
   ("rp" "" "--reinstall-package=")]]
  [["Cache Options"
   (uv--no-cache)
   (uv--refresh)]
  ["UV Python Options"
   (uv-python-choice)]]
  [["UV init command"
    (uv-add-command)]])

(transient-define-prefix uv-init-menu ()
  "UV init transient interface."
  :init-value 'uv-init-prefix-init
  ["Arguments"
   (uv-path--path) ; Done
   ("na" "name of the project" "--name=")
   ("pk" "set up proj as package" "--package") ; Done
   ("nr" "do not create a readme file" "--no-readme") ; Done
   ("cf" "configuration file path" "--config-file=")]
  ["UV Python Options"
   (uv-python-choice)]
  [["UV init command"
    (uv-init-command)]])

(transient-define-argument uv-locked-choice ()
  :class 'transient-option
  :shortarg "lk"
  :description "Assert that the `uv.lock` will remain unchanged"
  :argument "--locked")

(transient-define-argument uv-frozen-choice ()
  :class 'transient-option
  :shortarg "fr"
  :description "Sync without updating the `uv.lock` file"
  :argument "--frozen")

(transient-define-prefix uv-sync-menu ()
  "UV sync transient interface."
  :init-value 'uv-init-prefix-init
  ["Options"
   ("e" "Include optional dependencies from the extra group name" "--extra=")
   ("ae" "Include all optional dependencies" "--all-extras") ; Done
   ("nd" "Omit development dependencies" "--no-dev") ; Done
   ("od" "Omit non-development dependencies" "--only-dev")
   (uv-locked-choice)
   (uv-frozen-choice)
   ("nip" "Do not install the current project" "--no-install-project")
   ("ne" "Do not install any workspace members, including the root project" "--no-editable")]
  ["UV Python Options"
   (uv-python-choice)]
  [["UV sync command"
    (uv-sync-command)]])

(transient-define-prefix uv-pip-list-menu ()
  "UV pip list transient interface."
  ;:init-value 'uv-init-prefix-init
  ["Options"
   ("e" "editable" "--editable")
   ("st" "strict" "--strict")
   ("sy" "system" "--system")
   ]
  [["UV sync command"
    (uv-pip-list-command)]])

(transient-define-prefix uv-pip-show-menu ()
  "UV pip show transient interface."
;:init-value 'uv-init-prefix-init
  [["Arguments"
    (uv--pip-show-package)]
  ["Options"
   ("st" "strict" "--strict")
   ("sy" "system" "--system")
   ]]
  [["Python Options"
    (uv-python-choice)
    (uv--no-cache)]]
  [["UV show command"
    (uv-pip-show-command)]])

(transient-define-suffix uv-pip-list ()
  :key "l"
  :description " uv"
  :transient t
  (interactive)
  (uv-pip-list-menu))

(transient-define-suffix uv-pip-show ()
  :key "s"
  :description " uv"
  :transient t
  (interactive)
  (uv-pip-show-menu))



(transient-define-prefix uv-pip-menu ()
  "UV sync transient interface."
  :init-value 'uv-init-prefix-init
  ["Commands"
   (uv-pip-list)
   (uv-pip-show)]
   ;; ("install")
   ;; ("uninstall")
   ;; ("sync")]
  ["UV Python Options"
   (uv--no-cache)
   ])

(transient-define-suffix uv-sync-command (&optional args)
  :key "s"
  :description "UV sync command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-sync-args-str (uv--gen-format-string 8))
	 (uv-python-version (uv-parse-arg-flag "--python=" args))
	 (uv-extra (uv-parse-arg-flag "--extra=" args))
	 (uv-all-extras (uv-parse-arg-flag "--all-extras" args))
	 (uv-no-dev (uv-parse-arg-flag "--no-dev" args))
	 (uv-only-dev (uv-parse-arg-flag "--only-dev" args))
	 (uv-locked (uv-parse-arg-flag "--locked" args))
	 (uv-frozen (uv-parse-arg-flag "--frozen" args))
	 (uv-no-install-project (uv-parse-arg-flag "--no-install-project" args))
	 (uv-no-editable (uv-parse-arg-flag "--no-editable" args))
	 (uv-sync-args (format uv-sync-args-str
			       uv-python-version
			       uv-extra
			       uv-all-extras
			       uv-no-dev
			       uv-only-dev
			       uv-locked
			       uv-frozen
			       uv-no-editable
			       uv-no-install-project)))
    (uv--execute-command "sync"  uv-sync-args "UV SYNC")))

(transient-define-suffix uv-init ()
  :key "i"
  :description "init new project with uv"
  :transient t
  (interactive)
  (uv-init-menu))

(transient-define-suffix uv-pip ()
  :key "p"
  :description "execute pip subcommands"
  :transient nil
  (interactive)
  (uv-pip-menu))

(transient-define-suffix uv-lock ()
  :key "l"
  :description "lock"
  :transient nil
  (interactive)
  (uv-lock-menu))

(transient-define-suffix uv-python ()
  :key "v"
  :description "python version management"
  :transient nil
  (interactive)
  (uv-python-menu))

(transient-define-suffix uv-sync ()
  :key "s"
  :description "sync environment"
  :transient nil
  (interactive)
  (uv-sync-menu))

(transient-define-suffix uv-add ()
  :key "a"
  :description "Add a dependency"
  :transient nil
  (interactive)
  (uv-add-menu))

(transient-define-suffix uv-venv ()
  :key "o"
  :description "venv environment creation"
  :transient nil
  (interactive)
  (uv-venv-menu))

(transient-define-prefix uv-menu ()
  "UV transient interface."
  [["New Project"
    (uv-init)
    (uv-venv)]
   ["Dependencies"
    (uv-add)
    (uv-python)
    (uv-lock)
    (uv-pip)
    (uv-sync)]])

(provide 'uv)
