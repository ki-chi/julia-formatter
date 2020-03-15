;;; julia-formatter.el --- Format Julia code using JuliaFormatter.jl -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Kiichi

;; Author: Kiichi <k.brilliant@gmail.com>
;; URL: https://github.com/ki-chi/julia-formatter.el
;; Version: 0.1.0
;; Package-Requires: ((json "1.3"))
;; Keywords: languages

;; MIT License

;;; Commentary:
;; For formatting Julia code, 'julia-formatter.el' provides the functions:
;; 'julia-format-region' and 'julia-format-buffer'.
;; There functions call the process of JuliaFormatter.jl [ https://github.com/domluna/JuliaFormatter.jl ].

;;; Code:

(require 'json)


(defgroup julia-formatter nil
  "Format code in Julia using JuliaFormatter.jl"
  :group 'tools)

(defcustom julia-formatter-el-dir-path
  (file-name-directory load-file-name)
  "The path of directory containing julia-formatter.el."
  :group 'julia-formatter
  :type '(string))

(defcustom julia-formatter-json-log nil
  "If non-nil, log JSON messages to and from the JuliaFormatter server to the *Message* buffer."
  :group 'julia-formatter
  :type '(boolean))

(defcustom julia-executable-options
  (list "--startup-file=no" (concat "--project=" (directory-file-name julia-formatter-el-dir-path)))
  "Command-line arguments for Julia."
  :group 'julia-formatter
  :type '(list string string))

(defcustom julia-formatter-server-path
  (concat julia-formatter-el-dir-path "scripts/server.jl")
  "The relative path of the server script from julia-formatter.el."
  :group 'julia-formatter
  :type '(file :must-match t)
  :risky t)

(defcustom julia-formatter-timeout-in-seconds 5
  "Time-out in seconds for formatting."
  :group 'julia-formatter
  :type '(integer))

(defcustom julia-executable
  (or (executable-find "julia") "julia")
  "Executable path of Julia."
  :group 'julia-formatter
  :type '(file :must-match t)
  :risky t)

(defcustom julia-formatter-process nil
  "The process running JuliaFormatter.jl."
  :group 'julia-formatter
  :type '(process))

(defcustom julia-formatter-process-name "julia-formatter"
  "The name of process running JuliaFormatter.jl."
  :group 'julia-formatter
  :type '(string))

(defcustom julia-formatter-json-output ""
  "JSON output returned by the server."
  :group 'julia-formatter
  :type '(string))

(defun julia-formatter-server-start ()
  "Run the server of JuliaFormatter.jl."
  (interactive)
  (unless (get-process julia-formatter-process-name)
    (setq julia-formatter-process
          (let ((process-connection-type nil))
            (apply #'(lambda (start-up-option project-path-option)
                       (start-process julia-formatter-process-name nil
                                      julia-executable start-up-option project-path-option
                                      julia-formatter-server-path))
                   julia-executable-options)))
    (set-process-query-on-exit-flag julia-formatter-process nil)
    (set-process-filter julia-formatter-process
                        '(lambda (process output)
                           (setq julia-formatter-json-output output)))
    (message "[JuliaFormatter] Start the server")))

(defun julia-format (code)
  "Format Julia CODE by sending it to the server."
  (julia-formatter-server-start)
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    ;; Send JSON to the server
    (let ((json-input (json-encode
                       `(("method" . "format")
                         ("status" . "")
                         ("params" ("text" . ,(split-string code "\n")))))))
      (when julia-formatter-json-log
        (message (concat "[JuliaFormatter][INPUT] " json-input)))
      (process-send-string julia-formatter-process (concat json-input "\n")))

    ;; Receive JSON from the server
    (let (output status)
      (accept-process-output julia-formatter-process julia-formatter-timeout-in-seconds)
      (if (equal julia-formatter-json-output "")
          (message (concat "[JuliaFormatter][INFO] Timeout in " (int-to-string julia-formatter-timeout-in-seconds) " sec.") )
        (progn ;; else
          (when julia-formatter-json-log
            (message (concat "[JuliaFormatter][OUTPUT] " julia-formatter-json-output)))
          (setq output (json-read-from-string julia-formatter-json-output))
          (setq julia-formatter-json-output "")  ;; clear
          (setq status (gethash "status" output))
          (cond ((equal status "success")
                 (message "[JuliaFormatter][INFO] Success")
                 (gethash "text" (gethash "params" output))) ; return the lines of formatted code
                ((equal status "error")
                 (message "[JuliaFormatter][ERROR] Error in formatting") nil)
                (t
                 (message "[JuliaFormatter][ERROR] Unexpected error") nil)))))))

(defun julia-format--insert (code)
  "Insert formatted CODE into the buffer."
  (let ((delim ""))
    (while code
      (insert (concat delim (car code)))
      (setq delim "\n")
      (setq code (cdr code)))))

(defun julia-format--replace (code start end)
  "Replace the region between START and END to CODE."
  (goto-char start)
  (delete-region start end)
  (julia-format--insert code))

(defun julia-format-region (start end)
  "Format the Julia code in the region between START and END."
  (interactive "r")
  (let ((code (buffer-substring-no-properties start end))
        (formatted-code nil))
    (setq formatted-code (julia-format code))
    (when formatted-code
      (julia-format--replace formatted-code start end))))

(defun julia-format-buffer ()
  "Format the entire Julia code in the buffer."
  (interactive)
  (let ((code (buffer-substring-no-properties (point-min) (point-max)))
        (formatted-code nil))
    (setq formatted-code (julia-format code))
    (when formatted-code
      (julia-format--replace formatted-code (point-min) (point-max)))))

(provide 'julia-formatter)

;;; julia-formatter.el ends here
