(require 'shell)

(defcustom rails-apps nil
  "An alist of rails apps' directory and port
ex: '((\"/path/to/app1\" 3000) (\"/path/to/app2\" 3001))"
  :type  'alist
  :group 'rails-server)

(defun comint-simple-send* (buffer-name command)
  (let ((buffer (shell buffer-name)))
    (set (make-local-variable 'comint-output-filter-functions)
         '(comint-truncate-buffer comint-postoutput-scroll-to-bottom ansi-color-process-output))
    (set (make-local-variable 'comint-buffer-maximum-size) autotest-maximum-lines)
    (set (make-local-variable 'comint-scroll-show-maximum-output) t)
    (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)
    (compilation-shell-minor-mode)
    (comint-simple-send buffer command)))

(defun port-already-in-use-p (port)
  (let ((cmd (concat "netstat -ltn | grep " (int-to-string port))))
    (not (string= (shell-command-to-string cmd) ""))))

(defun* rails-server-start ()
  "Start rails server"
   (interactive)
   (when (eq (length rails-apps) 0)
     (message "rails-apps is empty")
     (return-from rails-server-start))
   (dolist (app rails-apps)
     (let ((path (car app))
           (port (cadr app)))
       (unless (file-accessible-directory-p path)
         (message "Directory is not accessible")
         (return-from rails-server-start))
       (unless (file-exists-p (concat path "config/boot.rb"))
         (message "Not rails app dir?")
         (return-from rails-server-start))
       (let* ((paths (split-string (car app) "/"))
              (last (car (last paths)))
              (name (if (string= "" last) (nth (- (length paths) 2) paths) last))
              (buffer (concat "*rails-server-for-" name "*"))
              (port (int-to-string port))
              (cmd (concat "cd " path " && rails server -p " port)))
         (shell-command-to-string
          (concat "kill -9 `ps -ef | grep \"rails server -p " port
                  "\" | grep -v grep | awk '{print $2}'`"))
         (comint-simple-send* buffer cmd)))))

(provide 'rails-server)
