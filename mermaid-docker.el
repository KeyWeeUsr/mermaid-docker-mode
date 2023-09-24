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

(defconst mermaid-docker-tmp-folder
  "mermaid-docker"
  "Name for /tmp/<folder>")

(defconst mermaid-docker-git-repo
  "https://github.com/jihchi/mermaid.ink"
  "Address for the mermaid.ink repo")

(defconst mermaid-docker-image-name
  "md-ink-offline"
  "Name for mermaid-docker image")

(defconst mermaid-docker-header-size
  102400000
  "Node.js option --max-http-header-size")

(defconst mermaid-docker-port
  3000
  "Port mermaid-ink service listens on")

(defconst mermaid-docker-net
  "mermaid_no_internet"
  "Network name to use")

(defconst mermaid-docker-output
  ""
  "Default file output ('' / empty string)")

(defconst mermaid-docker-external-viewer-bin
  "/usr/bin/xviewer"
  "Path to external image viewer")

(defconst mermaid-docker-external
  nil
  "Use external viewer to display rendered mermaid graph")

(defun md-check-deps ()
  (inline)
  (message "Checking deps for mermaid-docker")
  (let ((buff-name "*mermaid-docker deps*")
        (failed nil)
        (net-name "mermaid-dummy"))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    ;; binaries
    (when (eq (executable-find "git") nil)
      (setq failed t)
      (save-excursion
        (switch-to-buffer (get-buffer-create buff-name))
        (insert "'git' not found\n")))

    (when (eq (executable-find "docker") nil)
      (setq failed t)
      (save-excursion
        (switch-to-buffer (get-buffer-create buff-name))
        (insert "'docker' not found\n")))

    (when (eq (executable-find "curl") nil)
      (setq failed t)
      (save-excursion
        (switch-to-buffer (get-buffer-create buff-name))
        (insert "'curl' not found\n")))

    (when (eq (executable-find "jq") nil)
      (setq failed t)
      (save-excursion
        (switch-to-buffer (get-buffer-create buff-name))
        (insert "'jq' not found\n")))

    ;; permissions, network, etc
    (when (not (eq 0 (call-process
                      "docker" nil
                      (get-buffer-create buff-name)
                      nil
                      "run" "--rm"
                      "hello-world:latest")))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))

    (when (not (eq 0 (call-process-region
                      "FROM scratch"
                      nil
                      "docker"
                      nil (get-buffer-create buff-name) nil
                      "build" "-")))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))

    (when (not (eq 0 (call-process
                      "docker" nil
                      (get-buffer-create buff-name)
                      nil
                      "network" "create" "--internal" net-name)))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))

    (when (not (eq 0 (call-process
                      "docker" nil
                      (get-buffer-create buff-name)
                      nil
                      "run" "--rm" (concat "--network=" net-name)
                      "hello-world:latest")))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))

    (when (not (eq 0 (call-process
                      "docker" nil
                      (get-buffer-create buff-name)
                      nil
                      "network" "rm" net-name)))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))

    (if (eq failed t)
        (progn
          (switch-to-buffer (get-buffer-create buff-name))
          (user-error "Some deps are missing"))
      (kill-buffer (get-buffer-create buff-name)))))

(defun md-create-temp-work-folder ()
  (inline)
  (message "Create temp work folder")
  (let ((name (concat
               (temporary-file-directory)
               mermaid-docker-tmp-folder)))
    (when (not (file-exists-p name))
      (make-directory name))))

(defun md-clone-mermaid-ink ()
  (inline)
  (message "Clone mermaid-ink")
  (let ((name (concat
               (temporary-file-directory)
               mermaid-docker-tmp-folder))
        (buff-name "*mermaid-docker clone*"))
    (if (file-exists-p (concat name "/.git"))
        (message "Skipping, already cloned")
      (if (eq 0 (call-process
                 "git" nil (get-buffer-create buff-name) nil
                 "clone" "--quiet" "--depth" "1"
                 mermaid-docker-git-repo name))
          (kill-buffer buff-name)
        (switch-to-buffer buff-name)))))

(defun md-build-docker-image ()
  (inline)
  (message "Build Docker image")
  (let ((name (concat
               (temporary-file-directory)
               mermaid-docker-tmp-folder))
        (buff-name "*mermaid-docker build*")
        (failed nil))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    (when (not (eq 0 (call-process
                      "docker" nil
                      (get-buffer-create buff-name)
                      nil
                      "build" "--tag" (concat mermaid-docker-image-name "-tmp")
                      name)))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))
    (if (eq failed t)
        (progn
          (switch-to-buffer (get-buffer-create buff-name))
          (user-error "Failed to build image"))
      (kill-buffer (get-buffer-create buff-name)))))

(defun md-initial-container-run ()
  (inline)
  (message "Initial container run (necessary ping to the Internet)")
  (let ((buff-name "*mermaid-docker initial run*")
        (cont-name "tmp-mermaid")
        (failed nil))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    ;; if mermaid-docker-image-name is not built
    ;; if tmp-mermaid not active
    (call-process "docker" nil (get-buffer-create buff-name) nil
                  "rm" "--force" cont-name)
    (when (not (eq 0 (call-process
                      "docker" nil
                      (get-buffer-create buff-name)
                      nil
                      "run" "--name" cont-name "--detach"
                      ;; Failed to move to new namespace: PID namespaces
                      ;; supported, Network namespace supported, but failed:
                      ;; errno = Operation not permitted
                      ;;
                      ;; Error: Failed to launch the browser process!
                      "--cap-add=SYS_ADMIN"
                      "--env" (concat
                               "NODE_OPTIONS=\"--max-http-header-size="
                               (format "%s" mermaid-docker-header-size) "\"")
                      "--publish" (concat "127.0.0.1:"
                                          (format "%s:%s"
                                                  mermaid-docker-port
                                                  mermaid-docker-port))
                      (concat mermaid-docker-image-name "-tmp"))))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))
    (if (eq failed t)
        (progn
          (switch-to-buffer (get-buffer-create buff-name))
          (user-error "Failed to run init container"))
      (kill-buffer (get-buffer-create buff-name)))))

(defun md-test-graph-rendering ()
  (inline)
  (message "Test graph rendering")
  (let ((name (concat
               (temporary-file-directory)
               mermaid-docker-tmp-folder))
        (buff-name "*mermaid-docker test graph*")
        (cont-name "tmp-mermaid")
        (failed nil))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    ;; if final image is not built
    (when (not (eq 0 (call-process
                       "curl" nil
                       (get-buffer-create buff-name)
                       nil
                       "--silent"
                       (concat
                        "http://127.0.0.1:"
                        (format "%s" mermaid-docker-port)
                        "/img/"
                        (base64-encode-string "graph LR;A-->B&C&D;")))))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))
    (when (not (eq 0 (call-process
                       "docker" nil
                       (get-buffer-create buff-name)
                       nil
                       "stop" cont-name)))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))
    (if (eq failed t)
        (progn
          (switch-to-buffer (get-buffer-create buff-name))
          (user-error "Failed to test graph rendering"))
      (kill-buffer (get-buffer-create buff-name)))))

(defun md-create-image-for-offline-mode ()
  (inline)
  (message "Create image for offline mode")
  (let ((name (concat
               (temporary-file-directory)
               mermaid-docker-tmp-folder))
        (buff-name "*mermaid-docker offline image*")
        (cont-name "tmp-mermaid")
        (failed nil))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    ;; if final image is not built
    (when (not (eq 0 (call-process
                       "docker" nil
                       (get-buffer-create buff-name)
                       nil
                       "commit" cont-name
                       mermaid-docker-image-name)))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))
    (call-process "docker" nil (get-buffer-create buff-name) nil
                  "rm" "--force" cont-name)
    (if (eq failed t)
        (progn
          (switch-to-buffer (get-buffer-create buff-name))
          (user-error "Failed to create offline image"))
      (kill-buffer (get-buffer-create buff-name)))))

(defun md-start-offline-mode ()
  (inline)
  (message "Start offline mode")
  (let ((name (concat
               (temporary-file-directory)
               mermaid-docker-tmp-folder))
        (buff-name "*mermaid-docker start offline*")
        (cont-name mermaid-docker-image-name)
        (net-name mermaid-docker-net)
        (failed nil))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    ;; if mermaid not active
    (call-process "docker" nil (get-buffer-create buff-name) nil
                  "rm" "--force" cont-name)
    (call-process "docker" nil (get-buffer-create buff-name) nil
                  "network" "rm" net-name)
    (when (not (eq 0 (call-process
                      "docker" nil
                      (get-buffer-create buff-name)
                      nil
                      "network" "create" "--internal" "--driver=bridge"
                      mermaid-docker-net)))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))
    (when (not (eq 0 (call-process
                      "docker" nil
                      (get-buffer-create buff-name)
                      nil
                      "run" "--name" cont-name "--detach"
                      ;; Failed to move to new namespace: PID namespaces
                      ;; supported, Network namespace supported, but failed:
                      ;; errno = Operation not permitted
                      ;;
                      ;; Error: Failed to launch the browser process!
                      "--cap-add=SYS_ADMIN"
                      "--env" (concat
                               "NODE_OPTIONS=\"--max-http-header-size="
                               (format "%s" mermaid-docker-header-size) "\"")
                      "--publish" (concat "127.0.0.1:"
                                          (format "%s:%s"
                                                  mermaid-docker-port
                                                  mermaid-docker-port))
                      (concat "--network=" mermaid-docker-net)
                      mermaid-docker-image-name)))
      (progn
        (switch-to-buffer (get-buffer-create buff-name))
        (setq failed t)))
    (if (eq failed t)
        (progn
          (switch-to-buffer (get-buffer-create buff-name))
          (user-error "Failed to start offline mode"))
      (kill-buffer (get-buffer-create buff-name)))))

(defun md-get-ip ()
  (inline)
  (string-replace
   "\n" ""
   (shell-command-to-string
    (concat "docker inspect "
            mermaid-docker-image-name
            " | jq -r .[].NetworkSettings.Networks."
            mermaid-docker-net
            ".IPAddress"))))

(defun md-test-graph-rendering-via-offline-mode ()
  (inline)
  (message "Test graph rendering via offline mode")
  (let ((out-file mermaid-docker-output)
        (out-buff "*mermaid-docker output*"))
    (when (string-equal "" out-file)
      (setq out-file (make-temp-file nil nil ".jpg" nil)))

    (url-copy-file
     (concat
      "http://" (md-get-ip) ":" (format "%s" mermaid-docker-port)
      "/img/"
      (base64-encode-string "graph LR;A-->B&C&D;"))
     out-file t)

    (when (string-equal "" out-file)
      (get-buffer-create out-buff)
      (save-excursion
        (switch-to-buffer out-buff)
        (insert-image (create-image out-file)))
      (delete-file out-file))))

(defun md-test-graph-rendering-via-external-editor ()
  (inline)
  (message "Test graph rendering via external editor")
  (let ((out-file (when (string-equal "" mermaid-docker-output)
                    "/tmp/mermaid.jpg"))
        (out-buff "*mermaid-docker output*"))
    (url-copy-file
     (concat
      "http://" (md-get-ip) ":" (format "%s" mermaid-docker-port)
      "/img/"
      (base64-encode-string "graph LR;A-->B&C&D;"))
     out-file t)
    (start-process
     "mermaid-docker-ext" nil
     mermaid-docker-external-viewer-bin
     out-file)))

(defun mermaid-docker-install ()
  "Install everything for mermaid-docker"
  (interactive)
  (md-check-deps)
  (md-create-temp-work-folder)
  (md-clone-mermaid-ink)
  (md-build-docker-image)
  (md-initial-container-run)
  (sleep-for 5)
  (md-test-graph-rendering)
  (md-create-image-for-offline-mode)
  (md-start-offline-mode)
  (sleep-for 2)
  (when (eq nil (md-test-graph-rendering-via-offline-mode))
    (progn
      (message "Failed to display in Emacs, trying external program")
      (md-test-graph-rendering-via-external-editor))))

(defun mermaid-docker-render-external (filename))

(defun mermaid-docker-render-internal (filename)
  (inline)
  (let ((out-file mermaid-docker-output)
        (out-buff "*mermaid-docker output*"))

    (when (string-equal "" out-file)
      (setq out-file (make-temp-file nil nil ".jpg" nil)))

    (url-copy-file
     (concat
      "http://" (md-get-ip) ":" (format "%s" mermaid-docker-port)
      "/img/"
      (base64-encode-string
       (with-temp-buffer (insert-file-contents filename) (buffer-string))))
     out-file t)

    (when (string-equal "" out-file)
      (get-buffer-create out-buff)
      (save-excursion
        (switch-to-buffer out-buff)
        (insert-image (create-image out-file)))
      (delete-file out-file))))

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
