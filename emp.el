(require 'dbus)

(defcustom mpris-service
  "org.mpris.MediaPlayer2.rhythmbox"
  "Default service for MPRIS"
  :type 'string)

(defcustom mpris-max-timeout
  2000
  "Default timeout for MPRIS service"
  :type 'integer)

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

(defun mpris-get-prop (prop-name)
  (mpris-call mpris-get-prop-i "Get" mpris-player-i prop-name))
