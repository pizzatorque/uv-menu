(require 'transient)

(defvar uv-shell-command "uv --color never")

(defun uv-init-prefix-init (obj)
  (oset obj value `(,(format "--path=%s" (file-name-directory buffer-file-name)))))

(transient-define-argument uv-path--path ()
  "Environment Path Argument."
  :class 'transient-option
  :shortarg "p"
  :description "Environment path"
  :always-read t
  :argument "--path="
  :reader (lambda (_prompt _initial _history) (read-file-name _prompt)))

(defun uv-parse-arg-flag (flag args)
  "Return FLAG and arg from ARGS concatenated."
  (message (format "FLAG: %s" flag))
  (message (format "args:: %s" args))
  (message (format "transient value %s" (transient-arg-value flag args)))
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
	    (message "path")
	    (substring concatenated-flag 7))
	   (_
	    concatenated-flag))))))

(defun uv--gen-format-string (n)
  (let ((unit "%s "))
    (dotimes (n 5) (setq unit (concat "%s " unit)))
    unit))
    

(transient-define-suffix uv-init-command (&optional args)
  :key "t"
  :description "UV init command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-init-args-str (uv--gen-format-string 5)
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
    (message (format "this is the option %s" uv-init-args))
    (compilation-start (concat uv-shell-command " " "init" " " uv-init-args)
		       nil
		       (lambda (_) (format "UV INIT %s" uv-proj-path)))))


(transient-define-prefix uv-init-menu ()
  "UV init transient interface."
  :init-value 'uv-init-prefix-init
  ["Arguments"
   (uv-path--path) ; Done
   ("--name" "name of the project" "--name=")
   ("--package" "set up proj as package" "--package" :always-read t) ; Done
   ("--no-readme" "do not create a readme file" "--no-readme") ; Done
   ("-p" "python version" "--python=")
   ("--config-file" "configuration file path" "--config-file=")]
  [["UV init command"
    (uv-init-command)]])

(transient-define-prefix uv-sync-menu ()
  "UV sync transient interface."
  :init-value 'uv-init-prefix-init
  ["Options"
   ("--extra" "" "--extra=")
   ("--all-extras" "" "--all-extras" :always-read t) ; Done
   ("--no-dev" "" "--no-dev") ; Done
   ("--only-dev" "" "--only-dev")
   ("--locked" "" "--locked")
   ("--frozen" "" "--frozen")
   ("--no-install-project" "" "--no-install-project")]
  [["UV sync command"
    (uv-sync-command)]])

(transient-define-suffix uv-sync-command (&optional args)
  :key "s"
  :description "UV sync command options"
  :transient nil
  (interactive (list (transient-args transient-current-command)))
  (let* ((uv-sync-args-str (uv--gen-format-string 6))
	 (uv-extra (uv-parse-arg-flag "--extra=" args))
	 (uv-all-extras (uv-parse-arg-flag "--all-extras" args))
	 (uv-no-dev (uv-parse-arg-flag "--no-dev" args))
	 (uv-only-dev (uv-parse-arg-flag "--only-dev" args))
	 (uv-locked (uv-parse-arg-flag "--locked" args))
	 (uv-frozen (uv-parse-arg-flag "--frozen" args))
	 (uv-no-install-project (uv-parse-arg-flag "--no-install-project" args))
	 (uv-sync-args (format uv-sync-args-str
			       uv-extra
			       uv-all-extras
			       uv-no-dev
			       uv-only-dev
			       uv-locked
			       uv-frozen
			       uv-no-install-project)))
    (compilation-start (concat uv-shell-command " " "sync" " " uv-sync-args)
		       nil
		       (lambda (_) (format "UV SYNC")))))

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
  (message "Executed pip!"))

(transient-define-suffix uv-sync ()
  :key "s"
  :description "sync environment"
  :transient nil
  (interactive)
  (uv-sync-menu))

(transient-define-prefix uv-menu ()
  "UV transient interface."
  ;:init-value 'uv-prefix-init
  [["New Project"
   (uv-init)]
   ["Dependencies"
    (uv-pip)
    (uv-sync)]])

(provide 'uv)
