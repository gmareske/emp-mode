(require 'dbus)

(define-minor-mode emp-mode
  "Minor mode for emp, an emacs MPRIS interface"
  :group 'emp
  :lighter " EMP"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c n") 'emp-next)
	    (define-key map (kbd "C-c p") 'emp-prev)
	    (define-key map (kbd "C-c m") 'emp-toggle)
	    map))

(defgroup emp
  nil
  "Custom variables for emp, an emacs MPRIS interface")

(defcustom mpris-service
  "org.mpris.MediaPlayer2.rhythmbox"
  "Default service for MPRIS"
  :type 'string
  :group 'emp)

(defcustom mpris-max-timeout
  2000
  "Default timeout for MPRIS service"
  :type 'integer
  :group 'emp)

(defvar mpris-player-i "org.mpris.MediaPlayer2.Player")
(defvar mpris-get-prop-i "org.freedesktop.DBus.Properties")
(defvar mpris-player-name (clean-mpris-player-name mpris-service))

(defun find-mpris-player ()
  (let ((players
	 (seq-filter (lambda (s) (cl-search "org.mpris.MediaPlayer2" s))
		     (dbus-call-method :session "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames"))))
    players))

(defun clean-mpris-player-name (name)
  (cadr (split-string name "org.mpris.MediaPlayer2.")))

(defun mpris-call (interface method &rest args)
  ;; synchronous
  (apply 'dbus-call-method
	 (append `(:session
		   ,mpris-service
		   "/org/mpris/MediaPlayer2"
		   ,interface
		   ,method)
		 args)))

(defun mpris-get-prop (prop-name &rest args)
  (apply 'mpris-call
	 (append `(,mpris-get-prop-i
		   "Get"
		   ,mpris-player-i
		   ,prop-name)
		 args)))

(defun mpris-call-player (method &rest args)
  (apply 'mpris-call
	 (append `(,mpris-player-i
		   ,method)
		 args)))

;;; Interactive Functions
(defun emp-toggle ()
  (interactive)
  (message "Toggling %s..." mpris-player-name)
  (mpris-call-player "PlayPause"))

(defun emp-play ()
  (interactive)
  (message "Playing %s..." mpris-player-name)
  (mpris-call-player "Play"))

(defun emp-pause ()
  (interactive)
  (message "Pausing %s..." mpris-player-name)
  (mpris-call-player "Pause"))

(defun emp-next ()
  (interactive)
  (message "Starting next song on %s..." mpris-player-name)
  (mpris-call-player "Next"))

(defun emp-prev ()
  (interactive)
  (message "Starting previous song on %s..." mpris-player-name)
  (mpris-call-player "Previous"))

(defun emp-stop ()
  (interactive)
  (message "Stopping %s..." mpris-player-name)
  (mpris-call-player "Stop"))
   
       
(car (mpris-get-prop "Metadata"))
(defun metadata-hash ()
  (letrec ((helper (lambda (cur build)
		     (if (null cur)
			 build
		       (puthash (make-symbol (caar cur))
				(caadar cur)
				build)
		       (funcall helper (cdr cur) build)))))

    (funcall helper (car (mpris-get-prop "Metadata")) (make-hash-table :test 'eq))))

