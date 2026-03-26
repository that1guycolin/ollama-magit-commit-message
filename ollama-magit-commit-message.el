;;; ollama-magit-commit-message.el --- Integrate tavernari/git-commit-message with Magit  -*- lexical-binding: t; -*-
;; Copyright (C) 2026, Loeffler, Colin (that1guycolin)
;; Created date: 2026-03-05

;; Author: Loeffler, Colin <that1guycolin@gmail.com>
;; URL: https://github.com/that1guycolin/ollama-magit-commit-message
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.5.0") (transient "0.12.0"))
;; Keywords: convenience docs local tools

;;; Commentary:
;;
;; DESCRIPTION
;; -----------
;; Integrates tavernari/git-commit-message (Ollama) with Magit.
;; Adds an entry to the magit-commit transient that asynchronously generates
;; a commit message and places it in the COMMIT_EDITMSG buffer.
;;
;; INSTALLATION
;; ------------
;; Package is not currently available in MELPA, so these methods are hacky.
;;
;; Using Elpaca
;;
;;   (elpaca (ollama-magit-commit-message
;;            :host github
;;            :repo that1guycolin/ollama-magit-commit-message
;;            :files (:defaults)))
;;
;;   (use-package ollama-magit-commit-message
;;    :ensure (ollama-magit-commit-message
;;  	       :host github
;;  	       :repo that1guycolin/ollama-magit-commit-message
;;             :files (:defaults))
;;    :after magit)
;;
;; OR Place this file somewhere on your `load-path' and add to your init:
;;
;;   (require 'ollama-magit-commit-message)
;;
;; OR with use-package (:ensure nil will prevent pacakge.el from loading it):
;;
;;   (use-package ollama-magit-commit-message
;;     :ensure nil
;;     :after magit
;;     :load-path "~/.emacs.d/lisp/ollama-magit-gen-commit.el")
;;
;; Requirements:
;;   - magit
;;   - git-commit-message installed and on $PATH
;;     (https://ollama.com/tavernari/git-commit-message)
;;   - Ollama running locally with at least one model pulled
;;
;; USAGE
;; -----
;; In the Magit status buffer, press 'c' to open the commit transient.
;; Press 'g' to open the git-commit-message sub-transient
;; (OPTIONAL: use `-m' or `-p' to select a different model).
;;
;; Pressing 'g' in the sub-transient starts git-commit-message asynchronously.
;; Emacs stays fully responsive while the model runs.  When it finishes, the
;; Magit commit editor opens with the generated message already inserted.
;;
;; Available Models:
;;   - Mini (4b) (recommended for most use cases)
;;   - Default (8b)
;;   - Heavy (14b)

;;; Code:
(package-initialize)

(require 'magit)
(require 'magit-commit)
(require 'transient)
(require 'git-commit)


(defgroup ollama-magit-commit-message nil
  "Integrate tavernari/git-commit-message with Magit."
  :group 'magit
  :prefix "ollama-magit-commit-message-")

(defcustom ollama-magit-commit-message-executable "git-gen-commit"
  "Path or name of the tavernari/git-commit-message executable.
Even though the package is called git-commit-message, the executable is
typically called `git-gen-commit'.  If the executable is on your PATH,
the default \"git-gen-commit\" is fine.  Otherwise provide an absolute
path, e.g. \"/usr/local/bin/git-gen-commit\"."
  :type 'string
  :group 'ollama-magit-commit-message)

(defvar ollama-magit-commit-message--pending-message nil
  "Commit message waiting to be inserted into the next git-commit buffer.
Set by the process sentinel and consumed (then cleared) by
`ollama-magit-commit-message--insert-pending-message'.

nil when no message is pending.")

(defun ollama-magit-commit-message--insert-pending-message ()
  "Insert the pending git-gen-commit message into the current commit buffer.
Runs once via `git-commit-setup-hook', then removes itself from the hook
so it never fires a second time."
  (remove-hook 'git-commit-setup-hook
	       #'ollama-magit-commit-message--insert-pending-message)
  (when ollama-magit-commit-message--pending-message
    (save-excursion
      (goto-char (point-min))
      (insert ollama-magit-commit-message--pending-message)
      (unless (string-suffix-p "\n"
			       ollama-magit-commit-message--pending-message)
	(insert "\n")))
    (goto-char (point-min))
    (end-of-line)
    (setq ollama-magit-commit-message--pending-message nil)))

(defun ollama-magit-commit-message--make-sentinel (repo-root)
  "Return a process sentinel that captures REPO-ROOT.
The sentinel handles the exit of git-gen-commit.
REPO-ROOT is the absolute path to the Git repository root, captured at the
time `ollama-magit-commit-message-generate-commit-message' is called."
  (lambda (proc event)
    (let ((output-buf (process-buffer proc)))
      (cond
       ((string= event "finished\n")
	(let ((msg (with-current-buffer output-buf
		     (string-trim (buffer-string)))))
	  (kill-buffer output-buf)
	  (if (string-empty-p msg)
	      (message "Ollama-magit-commit-message: \
git-gen-commit produced no output — nothing to insert")
	    (message "Ollama-magit-commit-message: \
message generated, opening commit editor…")
	    (setq ollama-magit-commit-message--pending-message msg)
	    (add-hook 'git-commit-setup-hook
		      #'ollama-magit-commit-message--insert-pending-message)
	    (let ((default-directory repo-root))
	      (magit-commit-create '())))))

       ((string-prefix-p "exited abnormally" event)
	(let ((err (with-current-buffer output-buf
		     (string-trim (buffer-string)))))
	  (kill-buffer output-buf)
	  (message "Ollama-magit-commit-message: git-gen-commit failed — %s"
		   (if (string-empty-p err) event err))))

       (t
	(when (buffer-live-p output-buf)
	  (kill-buffer output-buf))
	(message "Ollama-magit-commit-message: \
git-gen-commit process ended unexpectedly (%s)"
		 (string-trim event)))))))

;;;###autoload
(defun ollama-magit-commit-message-generate-commit-message (&optional args)
  "Generate a commit message with git-gen-commit.  Open the Magit commit editor.

ARGS is the list of transient arguments from
`ollama-magit-commit-message-transient'.  If ARGS contains
\"--model=mini\" or \"--model=pro\", the respective model is used;
otherwise the default (8B) model is used.

The function is non-blocking: git-gen-commit runs as a subprocess and
Emacs remains fully responsive.  The commit editor opens automatically
when the model finishes."

  (interactive (list (transient-args 'ollama-magit-commit-message-transient)))
  (unless (executable-find ollama-magit-commit-message-executable)
    (user-error "Ollama-magit-commit-message: cannot find `%s' on PATH — \
is git-gen-commit installed?"
		ollama-magit-commit-message-executable))
  (let* ((use-mini      (member "--model=mini" args))
	 (use-pro       (member "--model=pro" args))
	 (model         (cond
			 (use-mini "mini")
			 (use-pro "pro")
			 (t "default")))
	 (repo-root     (or (magit-toplevel)
			    (user-error "Ollama-magit-commit-message: \
not inside a Git repository"))))
    (message "Ollama-magit-commit-message: \
running git-gen-commit (%s model)…" model)
    (let ((default-directory repo-root))
      (make-process
       :name     "git-gen-commit"
       :buffer   (generate-new-buffer " *ollama-magit-commit-message-output*")
       :command  (list ollama-magit-commit-message-executable
		       "--only-message"
		       "--model" model)
       :sentinel (ollama-magit-commit-message--make-sentinel repo-root)))))

(transient-define-prefix ollama-magit-commit-message-transient ()
  "Generate a Git commit message using tavernari/git-gen-commit (Ollama).

Default (8B, 40k context) is used by default.  However, the creator of
git-commit-message recommends using mini (4B, 256k context — fast and
suitable for most diffs) for most commits.  Toggle -m to use it.

Toggle -d to use the pro (14B, 40k context) model for more accurate messages on
complex or large diffs, or on commits involving many files."

  ["Options:"
   ("-m" "Use mini (4B) model" "--model=default")
   ("-p" "Use pro (14B) model" "--model=pro")
   "Default (8b) model is used if neither are specified."]
  ["Generate"
   ("g" "Generate commit message"
    ollama-magit-commit-message-generate-commit-message)])

(transient-append-suffix 'magit-commit '(1 -1)
  '("g" "Generate message (git-gen-commit)"
    ollama-magit-commit-message-transient))

(provide 'ollama-magit-commit-message)
;;; ollama-magit-commit-message.el ends here
