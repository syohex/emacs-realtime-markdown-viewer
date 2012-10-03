;;; realtime-markdown-viewer.el ---

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-realtime-markdown-viewer

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

(eval-when-compile
  (require 'cl))

(require 'websocket)

(defgroup realtime-markdown-viewer nil
  "Realtime Markdown Viewer"
  :group 'text
  :prefix "rtmv:")

(defcustom rtmv:port 5021
  "Port number for Plack App"
  :type 'integer
  :group 'reltime-markdown-viewer)

(defvar rtmv:websocket)

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
           :on-close (lambda (websocket) (setq wstest-closed t))))))

(defun rtmv:send-to-server ()
  (when realtime-markdown-viewer-mode
    (let ((str (buffer-substring-no-properties (point-min) (point-max))))
      (websocket-send-text rtmv:websocket str))))

(defvar rtmv:plackup-process nil)

(defvar rtmv:psgi-file "realtime-md-server.psgi"
  "File name of Realtime markdown viewer app")

(defvar rtmv:psgi-path
  (when load-file-name
    (let ((installed-dir (file-name-directory load-file-name)))
      (concat installed-dir rtmv:psgi-file)))
  "PSGI full path")

(defun rtmv:plackup (port)
  (let ((cmd (format "plackup --port %d %s" port rtmv:psgi-path)))
    (setq rtmv:plackup-process
          (start-process-shell-command "plackup" "*plackup*" cmd))))

(defun rtmv:init ()
  (let ((port rtmv:port))
    (rtmv:plackup port)
    (sleep-for 1)
    (rtmv:init-websocket port)
    (add-hook 'post-command-hook 'rtmv:send-to-server nil t)))

(defun rtmv:finalize ()
  (websocket-close rtmv:websocket)
  (remove-hook 'post-command-hook 'rtmv:send-to-server t))

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
