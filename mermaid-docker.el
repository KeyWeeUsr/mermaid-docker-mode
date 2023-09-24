;;; mermaid-docker.el

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: lisp, docker, mermaid, mmd, graph, design, jpg, image, api
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library attempts to create Mermaid graphs via mermaid-ink aka mermaid
;; as an API via a custom locally-built Docker image with restricted network
;; access, so that you are sure nothing gets out and your system is kept
;; isolated from random Node.js deps/files noise all around the filesystem.

;;; Code:

(defconst mermaid-docker-external
  nil
  "Use external viewer to display rendered mermaid graph")

(defun md-check-deps ()
  (inline)
  (message "Checking deps for mermaid-docker")
  (let ((buff-name "*mermaid-docker deps*")
        (failed nil))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    ;; binaries
    (when (eq (executable-find "git") nil)
      (setq failed t)
      (save-excursion
        (switch-to-buffer (get-buffer-create buff-name))
        (insert "'git' not found\n")))

    (if (eq failed t)
        (progn
          (switch-to-buffer (get-buffer-create buff-name))
          (user-error "Some deps are missing"))
      (kill-buffer (get-buffer-create buff-name)))))

(defun mermaid-docker-install ()
  "Install everything for mermaid-docker"
  (interactive)
  (md-check-deps)
  (message "md-create-temp-work-folder")
  (message "md-clone-mermaid-ink")
  (message "md-build-docker-image")
  (message "md-initial-container-run")
  (message "md-test-graph-rendering")
  (message "md-create-image-for-offline-mode")
  (message "md-start-offline-mode")
  (message "md-test-graph-rendering-via-offline-mode")
  (message "md-test-graph-rendering-via-external-editor"))

(defun mermaid-docker-render-external (filename))

(defun mermaid-docker-render-internal (filename))

(defun mermaid-docker-compile-file (filename)
  "Generic advice func which replaces 'mermaid-compile-file'"
  (if mermaid-docker-external
      (mermaid-docker-render-external filename)
    (mermaid-docker-render-internal filename)))

(defun mermaid-docker-mode-activate ()
  "Activate TypewriterRoll locally to a buffer"
  (inline)
  (advice-add
   'mermaid-compile-file
   :override #'mermaid-docker-compile-file))

(defun mermaid-docker-mode-deactivate ()
  "Dectivate TypewriterRoll locally to a buffer"
  (inline)
  (advice-remove
   'mermaid-compile-file
   #'mermaid-docker-compile-file))

(define-minor-mode mermaid-docker-mode
  "Minor mode to patch mermaid-mode with Docker-enabled version"
  :lighter " mermaid-docker"
  (if mermaid-docker-mode
      (mermaid-docker-mode-activate)
    (mermaid-docker-mode-deactivate)))

(provide 'mermaid-docker)
;;; mermaid-docker.el ends here
