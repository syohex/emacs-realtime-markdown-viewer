;;; realtime-markdown-viewer.el --- Real time markdown viewer with WebSocket

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-realtime-markdown-viewer
;; Version: 0.01
;; Package-Requires: ((cl-lib "0.5") (websocket "1.4"))

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

;;; Code:

(require 'cl-lib)
(require 'websocket)

(defgroup realtime-markdown-viewer nil
  "Realtime Markdown Viewer"
  :group 'text
  :prefix "rtmv:")

(defcustom rtmv:port 5021
  "Port number for Plack App"
  :type 'integer
  :group 'realtime-markdown-viewer)

(defcustom rtmv:lang 'perl
  "Language WebApp program"
  :type '(choice (const :tag "Amon2 Web Application" perl)
                 (const :tag "Sinatra Web Application") ruby)
  :group 'realtime-markdown-viewer)

(defvar rtmv:websocket)

(defvar rtmv:module-path (file-name-directory load-file-name))

(defun rtmv:init-websocket (port)
  (let ((url (format "ws://0.0.0.0:%d/emacs" port)))
    (message "Connect to %s" url)
    (setq rtmv:websocket
          (websocket-open
           url
           :on-message (lambda (websocket frame)
                         (message "%s" (websocket-frame-payload frame)))
           :on-error (lambda (ws type err)
                       (message "error connecting"))
           :on-close (lambda (_websocket))))))

(defun rtmv:send-to-server ()
  (when realtime-markdown-viewer-mode
    (let ((str (buffer-substring-no-properties (point-min) (point-max))))
      (websocket-send-text rtmv:websocket str))))

(defvar rtmv:webapp-process nil)

(defvar rtmv:psgi-file "realtime-md-server.psgi"
  "File name of Realtime markdown viewer app")

(defvar rtmv:sinatra-file "realtime_markdown_viewer.rb"
  "File name of Realtime markdown viewer app")

(defun rtmv:webapp-path ()
  (cl-case rtmv:lang
    (perl (concat rtmv:module-path rtmv:psgi-file))
    (ruby (concat rtmv:module-path rtmv:sinatra-file)))
  "WebApp full path")

(defun rtmv:webapp-launch-command (port)
  (cl-case rtmv:lang
    (perl (format "plackup --port %d %s" port rtmv:psgi-file))
    (ruby (format "bundle exec ruby %s -p %d" rtmv:sinatra-file port))))

(defun rtmv:webapp-launch (port)
  (when (not rtmv:webapp-process)
    (let ((cmd (rtmv:webapp-launch-command port))
          (default-directory rtmv:module-path))
      (setq rtmv:webapp-process
            (start-process-shell-command "rtmv" "*realtime markdown*" cmd)))))

(defun rtmv:kill-process ()
  (when rtmv:webapp-process
    (kill-process rtmv:webapp-process)
    (setq rtmv:webapp-process nil)))

(defun rtmv:init ()
  (let ((port rtmv:port))
    (rtmv:webapp-launch port)
    (sleep-for 1)
    (rtmv:init-websocket port)
    (add-hook 'kill-emacs-hook 'rtmv:kill-process)
    (add-hook 'post-command-hook 'rtmv:send-to-server nil t)))

(defun rtmv:finalize ()
  (websocket-close rtmv:websocket)
  (remove-hook 'post-command-hook 'rtmv:send-to-server t)
  (rtmv:kill-process))

;;;###autoload
(define-minor-mode realtime-markdown-viewer-mode
  "Realtime Markdown Viewer mode"
  :group      'realtime-markdown-viewer
  :init-value nil
  :global     nil
  (if realtime-markdown-viewer-mode
      (rtmv:init)
    (rtmv:finalize)))

(provide 'realtime-markdown-viewer)

;;; realtime-markdown-viewer.el ends here
