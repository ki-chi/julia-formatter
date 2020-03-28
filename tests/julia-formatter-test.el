(add-to-list 'load-path ".")
(require 'ert)
(require 'julia-formatter)

;; (eval-when-compile (require' f))
(defun read-text (filepath)
  (with-temp-buffer
  (insert-file-contents filepath)
  (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest jf-test-format ()
  "Test of `julia-format`, the core of the package's functions"
  (let ((unformatted-code (read-text "tests/cases/unformatted01.jl"))
        (expected (read-text "tests/cases/expected01.jl")))
    (julia-formatter-server-start)
    (sleep-for 10)
    (setq actual (mapconcat #'identity (julia-format unformatted-code) "\n"))
    (should (equal actual expected))))
