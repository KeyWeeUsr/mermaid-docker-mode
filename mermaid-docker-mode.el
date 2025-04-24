;;; mermaid-docker-mode.el --- Render mermaid graphs with Docker service -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, docker, mermaid, mmd, graph, design, jpg, image, api
;; Version: 2.1.0
;; Package-Requires: ((emacs "26.1") (mermaid-mode "20230517.1527+"))
;; Homepage: https://github.com/KeyWeeUsr/mermaid-docker-mode

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

;; This library attempts to create Mermaid graphs via the official mermaid-cli
;; using the official Docker image and restricting network access, so that you
;; are sure nothing gets out and your system is kept isolated from random
;; Node.js deps/files noise all around the filesystem.

;;; Code:

(require 'mermaid-mode)

(defgroup mermaid-docker
  nil
  "Customization group for `mermaid-docker-mode'."
  :group 'convenience
  :group 'external
  :group 'extensions
  :group 'x)

;; customization values
(defcustom mermaid-docker-verbose
  t
  "Emit messages when something's happening in the background."
  :group 'mermaid-docker
  :type 'boolean)

(defcustom mermaid-docker-always-check-deps
  t
  "Always look up binaries, libraries and other required tools."
  :group 'mermaid-docker
  :type 'boolean)

(defcustom mermaid-docker-image-name
  "minlag/mermaid-cli"
  "Official mermaid-cli image."
  :group 'mermaid-docker
  :type 'string)

(defcustom mermaid-docker-image-tag
  "11.4.3-beta.30"
  "Tag for official mermaid-cli image."
  :group 'mermaid-docker
  :type 'string)

(defcustom mermaid-docker-output-format
  "png"
  "Output format for rendered diagram."
  :group 'mermaid-docker
  :type 'string)

(defcustom mermaid-docker-output
  ""
  "Default file output ('' / empty string)."
  :group 'mermaid-docker
  :type 'string)

(defcustom mermaid-docker-external-viewer-bin
  "/usr/bin/xviewer"
  "Path to external image viewer."
  :group 'mermaid-docker
  :type 'string)

(defcustom mermaid-docker-focus-steal-fix
  t
  "Should attempt to fix focus stealing?"
  :group 'mermaid-docker
  :type 'boolean)

(defcustom mermaid-docker-focus-steal-ms
  200
  "Milliseconds to wait before stealing focus back."
  :group 'mermaid-docker
  :type 'number)

(defcustom mermaid-docker-external
  nil
  "Use external viewer to display rendered mermaid graph."
  :group 'mermaid-docker
  :type 'boolean)

(defcustom mermaid-docker-stay-in-window
  nil
  "Stay in window with the diagram after rendering."
  :group 'mermaid-docker
  :type 'boolean)

(define-error 'mermaid-docker-error "Generic mermaid-docker-mode error")

(define-error 'mermaid-docker-render-error
  "Rendering error"
  'mermaid-docker-error)


;; private/helper funcs
(defun mermaid-docker--log (format-string &rest args)
  "Logging func for symbol `mermaid-docker-mode'.
Argument FORMAT-STRING passed to `message' func as the first arg.
Optional argument ARGS passed to `message' func as the second arg."
  (message (format "mermaid-docker: %s" format-string) args))

(defun mermaid-docker--check-bin (buff-name cmd)
  "Check if a binary is present on the system.
Argument BUFF-NAME destination to write failure to.
Argument CMD name of the checked binary."
  (unless (executable-find cmd)
    (save-window-excursion
      (switch-to-buffer (get-buffer-create buff-name))
      (insert (format "'%s' not found\n" cmd)))
    t))

(defun mermaid-docker--call-cmd (buff-name cmd-list &optional input stderr)
  "Call a command with optional INPUT piping and write result back.
Argument BUFF-NAME destination to write output to.
Argument CMD-LIST list of strings as a command+args to execute."
  (when (or (null input) (eq input t)) (setq input ""))
  (let ((args (list)))
    (dolist (item (list input nil (pop cmd-list) nil
                        (list (get-buffer-create buff-name) stderr) nil))
      (if (null args) (setq args (list item)) (push item args)))
    (dolist (arg cmd-list) (push arg args))
    (unless (eq 0 (apply #'call-process-region (reverse args))) t)))

(defun mermaid-docker--check-deps ()
  "Check if all deps are present on the system."
  (when mermaid-docker-verbose
    (mermaid-docker--log "Checking deps for mermaid-docker"))
  (let ((buff-name "*mermaid-docker deps*")
        (failed nil)
        (deps '("docker")))
    (unless (eq window-system 'ns)
      (push "wmctrl" deps))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    ;; binaries
    (dolist (item deps)
      (when (mermaid-docker--check-bin buff-name item) (setq failed t)))

    ;; permissions, network, etc
    (dolist
        (item (list
               (list nil "docker" "run" "--rm" "hello-world:latest")
               (list nil "docker" "run" "--rm" "--network=none"
                     "hello-world:latest")))
      (let ((tmp nil))
        (setq tmp (pop item))
        (when (mermaid-docker--call-cmd buff-name item tmp) (setq failed t))))

    (if (not failed)
        (kill-buffer (get-buffer-create buff-name))
      (switch-to-buffer (get-buffer-create buff-name))
      (user-error "Some deps are missing"))))

(defun mermaid-docker--test-graph-rendering ()
  "Test graph rendering."
  (when mermaid-docker-verbose
    (mermaid-docker--log "Test graph rendering"))
  (let ((buff-name "*mermaid-docker test graph*")
        (failed nil))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    (when (mermaid-docker--call-cmd
           (get-buffer-create buff-name)
           (list "docker" "run" "--rm" "--interactive"
                 (format "%s:%s"
                         mermaid-docker-image-name
                         mermaid-docker-image-tag)
                 "--input" "/dev/stdin"
                 "--output" "-"
                 "--outputFormat" mermaid-docker-output-format)
           "graph LR;A-->B&C&D;")
      (setq failed t))

    (if (not failed)
        (kill-buffer (get-buffer-create buff-name))
      (switch-to-buffer (get-buffer-create buff-name))
      (user-error "Failed to test graph rendering"))))

(defun mermaid-docker-install ()
  "Install everything for mermaid-docker."
  (interactive)
  (when mermaid-docker-always-check-deps
    (mermaid-docker--check-deps))
  (mermaid-docker--test-graph-rendering)
  (when mermaid-docker-verbose
    (mermaid-docker--log "Successfully installed")))

(defun mermaid-docker--render-external (filename)
  "Render a Mermaid graph via external program.
Argument FILENAME Diagram file."
  (let ((out-file (when (string-equal "" mermaid-docker-output)
                    (format "%s/mermaid.%s"
                            (temporary-file-directory)
                            mermaid-docker-output-format))))

    (with-temp-file out-file
      (when (mermaid-docker--call-cmd
             (current-buffer)
             (list "docker" "run" "--rm" "--interactive"
                   "--network=none"
                   (format "%s:%s"
                           mermaid-docker-image-name
                           mermaid-docker-image-tag)
                   "--input" "-"
                   "--output" "-"
                   "--outputFormat" mermaid-docker-output-format)
             (with-temp-buffer
               (insert-file-contents filename)
               (buffer-string))
             nil)
        (signal 'mermaid-docker-render-error nil))
      (insert (buffer-substring-no-properties (point-min) (point-max))))

    (start-process
     "mermaid-docker-ext" nil
     mermaid-docker-external-viewer-bin
     out-file)
    (when mermaid-docker-focus-steal-fix
      (sleep-for (/ mermaid-docker-focus-steal-ms 1000))
      (start-process "fix-focus-steal" nil "wmctrl" "-a" "emacs"))))

(defun mermaid-docker--render-internal (filename)
  "Render a Mermaid graph internally in Emacs.
Argument FILENAME Diagram file."
  (let* ((out-buff (format "*mermaid-docker output <%s>*"
                           (file-name-nondirectory filename)))
         (tmp-buff (format "*%s*" (make-temp-name "")))
         (diagram (with-temp-buffer
                    (insert-file-contents filename)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (get-buffer-create out-buff)

    (when (mermaid-docker--call-cmd
           tmp-buff
           (list "docker" "run" "--rm" "--interactive"
                 "--network=none"
                 (format "%s:%s"
                         mermaid-docker-image-name
                         mermaid-docker-image-tag)
                 "--input" "-"
                 "--output" "-"
                 "--outputFormat" mermaid-docker-output-format)
           diagram
           nil)
      (signal 'mermaid-docker-render-error nil))

    (save-window-excursion
      (switch-to-buffer out-buff)
      (when mermaid-docker-stay-in-window
        (delete-region (point-min) (point-max)))
      (insert-image
       (create-image
        (with-current-buffer tmp-buff
          (encode-coding-string
           (buffer-substring-no-properties (point-min) (point-max))
           'utf-8))
        nil
        t)))
    (unless mermaid-docker-stay-in-window
      (switch-to-buffer-other-window out-buff))))

(defun mermaid-docker-compile-file (filename)
  "Generic advice func to replace =mermaid-compile-file=.
Argument FILENAME =mermaid-compile-file= input arg."
  (if mermaid-docker-external
      (mermaid-docker--render-external filename)
    (mermaid-docker--render-internal filename)))

(defun mermaid-docker-mode--activate ()
  "Activate `mermaid-docker-mode` locally to a buffer."
  (advice-add
   'mermaid-compile-file
   :override #'mermaid-docker-compile-file))

(defun mermaid-docker-mode--deactivate ()
  "Dectivate `mermaid-docker-mode` locally to a buffer."
  (advice-remove
   'mermaid-compile-file
   #'mermaid-docker-compile-file))

;; public funcs
(defvar org-babel-default-header-args:mermaid-docker
  '((:file "file") (:exports . "results"))
  "Default arguments for evaluating a mermaid source block.")

(defun org-babel-execute:mermaid-docker (body params)
  "Execute command with BODY and PARAMS from src block."
  (let* ((out-file
          (or (cdr (assoc :file params))
              (error "Mermaid-docker requires a \":file\" header argument")))
         ;; TODO:
         ;; * mermaid syntax highlighting with mermaid-docker as dest
         ;; * insert to buffer as file
         ;; * (width (cdr (assoc :width params)))
         ;; * (height (cdr (assoc :height params)))
         ;; * (theme (cdr (assoc :theme params)))
         ;; * (background-color (cdr (assoc :background-color params)))
         )

    (with-temp-file out-file
      (when (mermaid-docker--call-cmd
             (current-buffer)
             (list "docker" "run" "--rm" "--interactive"
                   "--network=none"
                   (format "%s:%s"
                           mermaid-docker-image-name
                           mermaid-docker-image-tag)
                   "--input" "-"
                   "--output" "-"
                   "--outputFormat" mermaid-docker-output-format)
             body
             nil)
        (signal 'mermaid-docker-render-error nil))
      (insert (buffer-substring-no-properties (point-min) (point-max))))
    (format "file:%s" out-file)))

(define-minor-mode mermaid-docker-mode
  "Minor mode to patch `mermaid-mode' with Docker-enabled version."
  :lighter " mermaid-docker"
  (if mermaid-docker-mode
      (mermaid-docker-mode--activate)
    (mermaid-docker-mode--deactivate)))

(provide 'mermaid-docker-mode)

;;; mermaid-docker-mode.el ends here
