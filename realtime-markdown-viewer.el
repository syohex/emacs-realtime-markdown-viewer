;;; realtime-markdown-viewer.el ---

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL:

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
  :prefix 'rtmv:)

(defvar rtmv:websocket)

(defun rtmv:init-websocket ()
  (setq rtmv:websocket
        (websocket-open
         "ws://127.0.0.1:5000/emacs"
         :on-message (lambda (websocket frame)
                       (message "%s" (websocket-frame-payload frame)))
         :on-error (lambda (ws type err)
                     (message "error connecting"))
         :on-close (lambda (websocket) (setq wstest-closed t)))))

(defun rtmv:send-to-server ()
  (if realtime-markdown-viewer-mode
      (let ((str (buffer-substring-no-properties (point-min) (point-max))))
        (websocket-send-text rtmv:websocket
                             (encode-coding-string str 'raw-text)))))

(defun rtmv:init ()
  (rtmv:init-websocket)
  (add-hook 'post-command-hook 'rtmv:send-to-server nil t))

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
