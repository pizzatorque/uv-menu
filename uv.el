;; -*- lexical-binding: t; -*-

(require 'transient)
(require 'project)

(defvar uv-shell-command "uv --color=never")

(defgroup uv-commands nil "")

(defcustom uv-requirements-filename "requirements.txt"
  "Name of the requirements file."
  :group 'uv-commands
  :type '(string))

(defcustom uv-project-file "pyproject.toml"
  "Name of the package configuration file."
  :group 'uv-commands
  :type '(string))

(defcustom uv-venv-path ".venv"
  "Path to the project's venv."
  :group 'uv-commands
  :type '(string))

;; TODO
;; finish uv run
;; integrate with embark

(defun uv-init-prefix-init (obj)
  "Default prefix path for the init command.
OBJ is required by prefix."
  (oset obj value `(,(format "--path=%s" (cond
					     ((buffer-file-name)
					      (file-name-directory buffer-file-name))
					     ((project-current)
					      (project-root (project-current)))
					     (t default-directory))))))

;(defun uv--get-default-dir ())

;; refactor this
(defun uv-add-prefix-init (obj)
  "Default prefix for the add command.
OBJ is required by prefix."
  ;;; refactor this
  (let* ((req-file (cond ((file-exists-p
			   (concat (file-name-directory buffer-file-name)
				 uv-requirements-filename))
			  (concat (file-name-directory buffer-file-name)
				 uv-requirements-filename))
			((file-exists-p
			  (concat default-directory uv-requirements-filename))
			 (concat default-directory uv-requirements-filename))
			((and (project-current) (file-exists-p (concat (project-root (project-current)) uv-requirements-filename)))
			 (concat (project-root (project-current)) uv-requirements-filename))
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
  (oset obj value `(,(concat (format "--path=%s" (concat (cond
							  ((project-current)
							   (project-root (project-current)))
							  ((buffer-file-name)
							   (file-name-directory buffer-file-name))
							  (t default-directory))
							 uv-venv-path))))))

(transient-define-argument uv-path--path ()
  "Environment Path Argument."
  :class 'transient-option
  :shortarg "pt"
  :description "Environment root path"
  :always-read t
  :argument "--path="
  :reader (lambda (_prompt _initial _history) (read-file-name _prompt)))

(transient-define-argument uv-run--command ()
  "Run Command Argument."
  :class 'transient-option
  :shortarg "c"
  :description "Run Command"
  :always-read t
  :argument "--command=")


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
	    (substring concatenated-flag (length "--path=")))
	   ((guard (string= flag "--package="))
	    (substring concatenated-flag (length "--package=")))
	   ((guard (string= flag "--src="))
	    (substring concatenated-flag (length "--src=")))
	   ((guard (string= flag "--command="))
	    (substring concatenated-flag (length "--command=")))
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

(transient-define-prefix uv-python-test ()
  "UV python transient interface."
  ["Cache Options"])

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

(transient-define-argument uv-sync-requirements-choice ()
  "Requirements File Path Argument."
  :class 'transient-option
  :shortarg "src"
  :description "Include all packages listed in the given requirements.txt file."
  :argument "--src="
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
   ("ri" "" "--reinstall")
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

(transient-define-argument uv-no-dev-choice ()
  :class 'transient-option
  :shortarg "nd"
  :description  "Omit development dependencies"
  :argument "--no-dev")

(transient-define-prefix uv-sync-menu ()
  "UV sync transient interface."
  :init-value 'uv-init-prefix-init
  ["Options"
   ("e" "Include optional dependencies from the extra group name" "--extra=")
   ("ae" "Include all optional dependencies" "--all-extras") ; Done
   (no-dev-choice) ; Done
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

(transient-define-prefix uv-pip-sync-menu ()
  "UV pip sync transient interface."
;:init-value 'uv-init-prefix-init
  [["Arguments"
    (uv-sync-requirements-choice)]
   ["Options"
    ("nb" "" "--no-build")
    ("ny" "" "--no-binary")
    ("oy" "" "--only-binary")
    ("aer" "" "--allow-empty-requirements")
    ("naer" "" "--no-allow-empty-requirements")
    ("nsp" "Do not break system packages" "--no-break-system-packages")
    ("sy" "system" "--system")
   ]]
  [["Installer Options"
    ("ri"
     "Resintall all packages, regardless of whether they're already installed."
     "--reinstall")]]
  [["Python Options"
    (uv-python-choice)
    (uv--no-cache)]]
  [["UV show command"
    (uv-pip-sync-command)]])

(transient-define-suffix uv-pip-list ()
  :key "l"
  :description "list"
  :transient t
  (interactive)
  (uv-pip-list-menu))

(transient-define-suffix uv-pip-show ()
  :key "sh"
  :description " show"
  :transient t
  (interactive)
  (uv-pip-show-menu))

(transient-define-suffix uv-pip-sync ()
  :key "sy"
  :description "sync"
  :transient t
  (interactive)
  (uv-pip-sync-menu))

(transient-define-prefix uv-pip-menu ()
  "UV sync transient interface."
  :init-value 'uv-init-prefix-init
  ["Commands"
   (uv-pip-list)
   (uv-pip-show)
   ;; ("install")
   ;; ("uninstall")
   (uv-pip-sync)]
  ["UV Python Options"
   (uv--no-cache)
   ])

(transient-define-prefix uv-run-menu ()
  "UV run command transient interface"
  :init-value 'uv-init-prefix-init
  ["Command"
   (uv-run--command)
   (uv-run-command)]
  ["Run Options"
   (uv-locked-choice)
   (uv-frozen-choice)
   (uv-no-dev-choice)
   ("iso" "Run the command in an isolated virtual environment" "--isolated")])

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

(transient-define-suffix uv-run ()
  :key "r"
  :description "Run command menu"
  :transient nil
  (interactive)
  (uv-run-menu))

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
    (uv-sync)]
   ["Run"
    (uv-run)]])

(defmacro uv--generate-transient-suffix-command (name command-name key description  &rest re)
  "Generate a transient suffix command.
Name it with given NAME which executes COMMAND-NAME.
Command can be invoked with given KEY, displaying the given
DESCRIPTION.  Apply all RE params to the shell command."
  `(let* ((flags
           (mapcar (lambda (v)
                     (format "%s" v))
                   ',re))
	  (parsed-flags (mapcar (lambda (flag)
                                  (lambda (args)
                                    (uv-parse-arg-flag flag args)))
			        flags)))
     (transient-define-suffix ,(intern (format "%s" name)) (&optional args)
        :key ,key
        :description ,description
        (interactive (list (transient-args transient-current-command)))
        (let* ((uv-args (mapcar (lambda (func) (funcall func args)) parsed-flags)))
          (uv--execute-command ,command-name
                               (mapconcat (apply-partially 'format "%s") uv-args " ")
                               (format "UV %s" ',name))))))

(uv--generate-transient-suffix-command uv-pip-sync-command
					"pip sync"
					"ps"
					"UV pip sync command"
					"--python="
					"--no-cache"
					"--no-break-system-packages"
					"--system"
					"--reinstall"
					"--no-build"
					"--no-binary"
					"--allow-empty-requirements"
					"--no-allow-empty-requirements"
					"--python-platform"
					"--src="
					)

(uv--generate-transient-suffix-command uv-init-command "init" "t" "UV init"
				       "--path="
				       "--name="
				       "--package"
				       "--no-readme"
				       "--python="
				       "--config-file="
				       )


(uv--generate-transient-suffix-command uv-python-install-command "python install" "i" "UV python install"
				       "--no-cache"
				       "--refresh"
				       ;; TODO
				       ;; uv--read-python-version-choice

(uv--generate-transient-suffix-command uv-pip-list-command "pip list" "l" "UV pip list"
				       "--editable"
				       "--strict"
				       "--system"
				       "--no-cache"
				       "--python="
				       )

(uv--generate-transient-suffix-command uv-pip-show-command
				       "pip show"
				       "x"
				       "UV pip show"
				       "--package="
				       "--strict"
				       "--system"
				       "--no-cache"
				       "--python=")

(uv--generate-transient-suffix-command uv-venv-command "venv" "v" "UV venv"
				       "--path="
				       "--no-project"
				       "--seed"
				       "--relocatable"
				       "--system-site-packages"
				       "--system-strategy="
				       "--link-mode"
				       "--allow-existing"
				       "--keyring-provider"
				       "--python=")

(uv--generate-transient-suffix-command uv-lock-command "lock" "k" "UV lock"
				       "--locked"
				       "--frozen"
				       "--no-cache"
				       "--refresh"
				       "--python="
				       "--upgrade"
				       "--no-sources"
				       )

(uv--generate-transient-suffix-command uv-add-command "add" "a" "UV add"
				       "--package="
				       "--no-project"
				       "--seed"
				       "--dev"
				       "--optional="
				       "--editable"
				       "--no-sync"
				       "--upgrade"
				       "--upgrade-package="
				       "--reinstall"
				       "--reinstall-package="
				       "--no-cache"
				       "--refresh"
				       "--index-strategy="
				       "--keyring-provider"
				       "--python=")

(uv--generate-transient-suffix-command uv-run-command "run" "r" "Run command"
				       "--command=")
(provide 'uv)
