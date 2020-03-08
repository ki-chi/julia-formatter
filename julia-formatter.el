;;; julia-formatter.el --- Format Julia code using JuliaFormatter.jl -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Kiichi

;; Author: Kiichi <k.brilliant@gmail.com>
;; URL: 
;; Version: 0.01
;; Package-Requires: ((json))

;; MIT License

;;; Code:

(require 'json)
(setq julia-formatter-el-dir-path (file-name-directory load-file-name))

(defgroup julia-formatter-group nil
  "Format code in Julia using JuliaFormatter.jl")

(defcustom julia-formatter-server-path
  (concat julia-formatter-el-dir-path "scripts/server.jl")
  "The path of the server script"
  :group 'julia-formatter-group
  :type '(file :must-match t)
  :risky t)

(defcustom julia-executable
  (or (executable-find "julia") "julia")
  "Executable path of Julia"
  :group 'julia-formatter-group
  :type '(file :must-match t)
  :risky t)

(defcustom julia-formatter-process nil
  "The process running JuliaFormatter.jl"
  :group 'julia-formatter-group)

(defcustom julia-formatter-process-name "julia-formatter"
  "The name of process running JuliaFormatter.jl"
  :group 'julia-formatter-group
  :type '(string))

(defcustom julia-formatter-buffer nil
  "The buffer for julia-formatter"
  :group 'julia-formatter-group)

(defcustom julia-formatter-json-output ""
  "JSON output returned by the server"
  :group 'julia-formatter-group
  :type '(string))

(defun launch-julia-formatter ()
  (interactive)
  (setq julia-formatter-buffer (get-buffer-create "*Julia Formatter*"))
  (with-current-buffer julia-formatter-buffer)
  (start-process julia-formatter-process-name julia-formatter-buffer
                 julia-executable julia-formatter-server-path))

(defun julia-formatter-process-filter (process output)
  (setq julia-formatter-json-output output))

(defun julia-format (code)
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))

    ;; Send JSON to server
    (let (json-input)
      (setq julia-formatter-process (or (get-process julia-formatter-process-name) (launch-julia-formatter)))
      (set-process-filter julia-formatter-process 'julia-formatter-process-filter)
      (setq json-input
            (json-encode
             `(("method" . "format")
               ("status" . "")
               ("params" ("text" . ,(split-string code "\n"))))))
      (message json-input)
      (process-send-string julia-formatter-process (concat json-input "\n")))

    ;; Receive JSON from server
    ;; (message (concat "OUTPUT: " julia-formatter-json-output))
    (let (output status)
      (accept-process-output julia-formatter-process 5) ;; TODO
      (setq output (json-read-from-string julia-formatter-json-output))
      (setq status (gethash "status" output))
      (cond ((eq status "success")
             (gethash "text" (gethash "params" output))) ; return the lines of formatted code
            ((eq status "error")
             (message "[JuliaFormatter] Error in formatting."))
            (t
             (message "[JuliaFormatter] Unexpected error"))
            ))))

(defun julia-format-region ()
  (interactive)
  (let (code formatted-code)
    (setq code (buffer-substring-no-properties (region-beginning) (region-end)))
    (if-let ((formatted-code (julia-format code)))
        (progn
          (message "HOGEHOGE")
          (goto-char (region-beginning))
          (delete-region (region-beginning) (region-end))
          (while formatted-code
            (insert formatted-code)
            (setq formatted-code (cdr formatted-code)))))))

(defun julia-format-buffer ()
  (interactive)
  (let (code formatted-code)
    (setq code (buffer-substring-no-properties (point-min) (point-max)))
    (if-let (formatted-code (julia-format code))
        (progn
          (goto-char (point-min))
          (delete-region (point-min) (point-max))
          (while formatted-code
            (insert formatted-code)
            (setq formatted-code (cdr formatted-code)))))))

(provide 'julia-formatter)

;;; julia-formatter.el ends here
