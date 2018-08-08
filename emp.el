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
  (mpris-call-player "PlayPause"))

(defun emp-play ()
  (interactive)
  (mpris-call-player "Play"))

(defun emp-pause ()
  (interactive)
  (mpris-call-player "Pause"))

(defun emp-next ()
  (interactive)
  (mpris-call-player "Next"))

(defun emp-prev ()
  (interactive)
  (mpris-call-player "Previous"))

(defun emp-stop ()
  (interactive)
  (mpris-call-player "Stop"))
   
       
(car (mpris-get-prop "Metadata"))
(defun metadata-alist ()
  (letrec ((helper (lambda (cur build)
		     (if (null cur)
			 build
		       (cons (make-symbol (caar cur))
			     (cons (caadar cur)
				   (funcall helper (cdr cur) build)))))))

    (funcall helper (car (mpris-get-prop "Metadata")) '())))

(metadata-alist)
