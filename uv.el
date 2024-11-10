(require 'transient)
(require 'project)

(defvar uv-shell-command "uv --color=never")

(defun uv-init-prefix-init (obj)
  "Default prefix path for the init command.
OBJ is required by prefix."
  (oset obj value `(,(format "--path=%s" (if
					     buffer-file-name
					     (file-name-directory buffer-file-name)
					   default-directory)))))

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
  :description "Environment path"
  :always-read t
  :argument "--path="
  :reader (lambda (_prompt _initial _history) (read-file-name _prompt)))

(transient-define-argument uv--refresh ()
  "Refresh the cache."
  :shortarg "rf"
  :description "Refresh the cache"
  :argument "--refresh")

(transient-define-argument uv--no-cache ()
  "No Cache Argument."
  :shortarg "nc"
  :description "Do not use the cache"
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
  :key "l"
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
   ("l" "" "--locked")
   ("f" "" "--frozen")
   ]
  ["Resolver Options"
   ("u" "" "--upgrade")
   ("ns" "" "--no-sources")]
  ["Cache Options"
   (uv--no-cache)
   (uv--refresh)]
  ["UV Python Options"
   (uv-python-choice)]
  [["UV lock command"
    (uv-lock-command)]])

(transient-define-prefix uv-python-menu ()
  "UV python transient interface."
  ["Cache Options"
   (uv--no-cache)
   (uv--refresh)]
  [["UV python command"
    (uv-python-install-command)]])

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

(transient-define-suffix uv-venv-command (&optional args)
  :key "v"
  :description "UV venv command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-venv-args-str (uv--gen-format-string 4))
	 (uv-proj-path (uv-parse-arg-flag "--path=" args))
	 (uv-no-proj (uv-parse-arg-flag "--no-project" args))
	 (uv-seed (uv-parse-arg-flag "--seed" args))
	 (uv-allow-existing (uv-parse-arg-flag "--allow-existing" args))
	 (uv-python-version (uv-parse-arg-flag "--python=" args))
	 (uv-venv-args (format uv-venv-args-str
			       uv-no-proj
			       uv-python-version
			       uv-seed
			       uv-allow-existing
			       uv-proj-path)))
    (uv--execute-command "venv"  uv-venv-args (format "UV VENV %s" uv-proj-path))))

(defun uv--read-python-version-choice ()
  "Prompt for a python version to choose."
  (interactive)
  (let ((_ '(:annotation-function uv-python-set)))
	(completing-read "Choose: " (uv--get-available-python-versions))))

(transient-define-argument uv-python-choice ()
  "Execution Path Argument."
  :class 'transient-option
  :shortarg "py"
  :description "description"
  :argument "--python="
  :reader (lambda (_prompt _initial _history) (uv--read-python-version-choice)))

(transient-define-prefix uv-venv-menu ()
  :init-value 'uv-venv-prefix-init
  ["Arguments"
   (uv-path--path)
   ("n" "d" "--no-project")
   ("s" "d" "--seed")
   ("a" "d" "--allow-existing")]
  ["UV Python Options"
   (uv-python-choice)]
  [["UV venv command"
    (uv-venv-command)]])

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

(transient-define-prefix uv-sync-menu ()
  "UV sync transient interface."
  :init-value 'uv-init-prefix-init
  ["Options"
   ("e" "" "--extra=")
   ("ae" "" "--all-extras") ; Done
   ("nd" "" "--no-dev") ; Done
   ("od" "" "--only-dev")
   ("l" "" "--locked")
   ("f" "" "--frozen")
   ("nip" "" "--no-install-project")
   ("ne" "" "--no-editable")
   ("og" "" "--only-group=")
   ("g" "" "--group=")
   ("ap" "" "--all-packages")]
  ["UV Python Options"
   (uv-python-choice)]
  [["UV sync command"
    (uv-sync-command)]])

(transient-define-suffix uv-sync-command (&optional args)
  :key "s"
  :description "UV sync command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-sync-args-str (uv--gen-format-string 11))
	 (uv-python-version (uv-parse-arg-flag "--python=" args))
	 (uv-extra (uv-parse-arg-flag "--extra=" args))
	 (uv-all-extras (uv-parse-arg-flag "--all-extras" args))
	 (uv-no-dev (uv-parse-arg-flag "--no-dev" args))
	 (uv-only-dev (uv-parse-arg-flag "--only-dev" args))
	 (uv-locked (uv-parse-arg-flag "--locked" args))
	 (uv-frozen (uv-parse-arg-flag "--frozen" args))
	 (uv-no-install-project (uv-parse-arg-flag "--no-install-project" args))
	 (uv-no-editable (uv-parse-arg-flag "--no-editable" args))
	 (uv-only-group (uv-parse-arg-flag "--only-group=" args))
	 (uv-group (uv-parse-arg-flag "--group=" args))
	 (uv-all-packages (uv-parse-arg-flag "--all-packages" args))
	 (uv-sync-args (format uv-sync-args-str
			       uv-python-version
			       uv-extra
			       uv-all-extras
			       uv-no-dev
			       uv-only-dev
			       uv-locked
			       uv-frozen
			       uv-no-editable
			       uv-only-group
			       uv-group
			       uv-all-packages
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
  (message "Placeholder: Executed pip!"))

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
    (uv-python)
    (uv-lock)
    (uv-pip)
    (uv-sync)]])

(provide 'uv)
