;;; mermaid-docker-mode-tests.el -- tests for mermaid-docker-mode

;;; Code:

(require 'ert)
(require 'mermaid-docker-mode)

(ert-deftest mermaid-docker-mode-check-bin ()
  (with-temp-buffer
    (should (not (mermaid-docker--check-bin
                  (buffer-name (current-buffer)) "sh")))
    (should (string-equal "" (buffer-string))))
  (with-temp-buffer
    (let ((cmd "missing"))
      (should (mermaid-docker--check-bin
               (buffer-name (current-buffer)) cmd))
      (should (string-equal
               (format "'%s' not found\n" cmd)
               (buffer-string))))))

(provide 'mermaid-docker-mode-tests)

;;; mermaid-docker-mode-tests.el ends here
