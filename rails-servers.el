; rails-servers.el
; 
; Copyright (c) Tomoyuki Matsumoto
; BSD license - http://www.opensource.org/licenses/bsd-license.php
; 
; Configurations:
;  1. Make sure this elisp file is in your load-path
;  2. Add following code to your .emacs file:  
;     (require 'rails-servers)
;     (setq rails-servers-apps '(("/path/to/app1/" 3000) ("/path/to/app2/" 3001)))
;  3. `M-x rails-servers-start` to start rails servers

(require 'shell)

(defcustom rails-servers-apps nil
  "An alist of rails apps' directory and port
ex: '((\"/path/to/app1\" 3000) (\"/path/to/app2\" 3001))"
  :type  'alist
  :group 'rails-servers)

(defcustom rails-servers-maximum-lines 5000
  "Maximum number of lines to display"
  :type 'number
  :group 'rails-servers)

(defun comint-simple-send* (buffer-name command)
  (let ((buffer (shell buffer-name)))
    (set (make-local-variable 'comint-output-filter-functions)
         '(comint-truncate-buffer comint-postoutput-scroll-to-bottom ansi-color-process-output))
    (set (make-local-variable 'comint-buffer-maximum-size) rails-servers-maximum-lines)
    (set (make-local-variable 'comint-scroll-show-maximum-output) t)
    (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)
    (compilation-shell-minor-mode)
    (comint-simple-send buffer command)))

(defun port-already-in-use-p (port)
  (let ((cmd (concat "netstat -ltn | grep " (int-to-string port))))
    (not (string= (shell-command-to-string cmd) ""))))

(defun* rails-servers-start ()
  "Start rails server"
   (interactive)
   (when (eq (length rails-servers-apps) 0)
     (message "rails-servers-apps is empty")
     (return-from rails-servers-start))
   (dolist (app rails-servers-apps)
     (let ((path (car app))
           (port (cadr app)))
       (unless (file-accessible-directory-p path)
         (message (concat "Directory is not accessible: " path))
         (return-from rails-servers-start))
       (unless (file-exists-p (concat path "config/boot.rb"))
         (message "Not rails app dir?")
         (return-from rails-servers-start))
       (let* ((paths (split-string (car app) "/"))
              (buffer (concat "*rails-server:" path "*"))
              (port (int-to-string port))
              (cmd (concat "cd " path " && rails server -p " port)))
         (shell-command-to-string
          (concat "kill -9 `ps -ef | grep \"rails server -p " port
                  "\" | grep -v grep | awk '{print $2}'`"))
         (comint-simple-send* buffer cmd)))))

(provide 'rails-servers)
