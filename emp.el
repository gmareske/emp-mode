(require 'dbus)

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
(defun emp-play ()
  (interactive)
  (mpris-call-player "PlayPause"))

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
   
       
