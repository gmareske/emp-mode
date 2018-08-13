;;; emp.el --- Control music players from Emacs


;; Copyright (C) 2018 Griffin Mareske <gmareske@gmail.com>
;; Author: Griffin Mareske
;; License: MIT License (included)
;; URL: http://github.com/gmareske/emp-mode
;; Created: 2018
;; Version: 0.1
;; Keywords: music mpris

;;; Commentary:
;;
;; This module allows control of MPRIS2-compatible music players
;; through DBUS

(require 'dbus)

;;; Code:

(define-minor-mode emp-mode
  "Minor mode for emp, an emacs MPRIS interface"
  :group 'emp
  :lighter " EMP"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "M-p n") 'emp-next)
	    (define-key map (kbd "M-p p") 'emp-prev)
	    (define-key map (kbd "M-p m") 'emp-toggle)
	    (define-key map (kbd "M-p s") 'emp-stop)
	    (define-key map (kbd "M-p u") 'emp-pause)
	    (define-key map (kbd "M-p i") 'emp-play)
	    map))

(defgroup emp
  nil
  "Custom variables for emp, an emacs MPRIS interface")

(defcustom mpris-service
  "org.mpris.MediaPlayer2.rhythmbox"
  "Default service for MPRIS."
  :type 'string
  :group 'emp)

(defcustom mpris-max-timeout
  2000
  "Default timeout for MPRIS service."
  :type 'integer
  :group 'emp)

(defvar emp-mode-version "0.1.0"
  "Version number of emp-mode.")
(defvar mpris-player-i "org.mpris.MediaPlayer2.Player"
  "Name of the MediaPlayer2.Player instance.")
(defvar mpris-prop-i "org.freedesktop.DBus.Properties"
  "Name of the Freedesktop Properties Interface.")

(defun find-mpris-player ()
  "Return a list of running MPRIS-compatible players."
  ;; Function currently not used
  (let ((players
	 (seq-filter (lambda (s) (cl-search "org.mpris.MediaPlayer2" s))
		     (dbus-call-method :session "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames"))))
    players))

(defun clean-mpris-player-name (name)
  "Return the cleaned NAME from the mpris-player dbus name."
  (cadr (split-string name "org.mpris.MediaPlayer2.")))

(defvar mpris-player-name (clean-mpris-player-name mpris-service)
  "Shorthand for the actual name of the program being controlled.")

(defun mpris-call (interface method &rest args)
  "Call #METHOD with #INTERFACE on the MPRIS object specified by the custom variable ‘mpris-service’ and return the result."
  ;; synchronous
  (apply 'dbus-call-method
	 (append `(:session
		   ,mpris-service
		   "/org/mpris/MediaPlayer2"
		   ,interface
		   ,method)
		 args)))

(defun mpris-set-prop (prop-name val &rest args)
  "Set PROP-NAME to VAL, returns a value on success or nil/error otherwise."
  (apply 'dbus-set-property
	(append `(:session
		  ,mpris-service
		  "/org/mpris/MediaPlayer2"
		  ,mpris-player-i
		  ,prop-name
		  ,val)
		args)))

(defun mpris-get-prop (prop-name &rest args)
  "Get PROP-NAME from the current mpris player."
  (apply 'mpris-call
	 (append `(,mpris-prop-i
		   "Get"
		   ,mpris-player-i
		   ,prop-name)
		 args)))

(defun mpris-call-player (method &rest args)
  "Calls METHOD on the current mpris player."
  (apply 'mpris-call
	 (append `(,mpris-player-i
		   ,method)
		 args)))

(defun metadata-hash ()
  "Return a hash table of symbols of MPRIS properties to string values."
  (letrec ((helper (lambda (cur build)
		     (if (null cur)
			 build
		       (puthash (caar cur)
				(caadar cur)
				build)
		       (funcall helper (cdr cur) build)))))

    (funcall helper (car (mpris-get-prop "Metadata")) (make-hash-table :test 'equal))))

;; Seeking
(defun mpris-seek (offset-time-ms)
  "Seek OFFSET-TIME-MS in the current track playing in the current MPRIS player."
  (mpris-call mpris-player-i "Seek" :int64 time-ms))

(defun mpris-seek-forward (offset)
  "Seek OFFSET ms forward in current track in current MPRIS player."
  (mpris-seek offset))

(defun mpris-seek-backward (offset)
  "Seek OFFSET ms backward in current track in current MPRIS player."
  (mpris-seek (- offset)))

;; Info
(defun mpris-song-info-message ()
  "Return a string of info about current song playing in current MPRIS player."
  (let* ((data (metadata-hash))
	 (song (gethash "xesam:title" data))
	 (artist (gethash "xesam:artist" data))
	 (length (gethash "mpris:length" data))
	 (pos (car (mpris-get-prop "Position"))))
    (format "<%s - %s %0.2f%%>" song artist (/ pos (float length)))))

;;; Interactive Functions
(defun emp-toggle ()
  "Toggle play on MPRIS Player."
  (interactive)
  (mpris-call-player "PlayPause")
  (message "Toggling %s... %s" mpris-player-name
	   (mpris-song-info-message)))


(defun emp-play ()
  "Play on MPRIS Player."
  (interactive)
  (mpris-call-player "Play")
  (message "Playing %s... %s" mpris-player-name
	   (mpris-song-info-message)))


(defun emp-pause ()
  "Pause on MPRIS Player."
  (interactive)
  (mpris-call-player "Pause")
  (message "Pausing %s... %s" mpris-player-name
	   (mpris-song-info-message)))


(defun emp-next ()
  "Next song on MPRIS Player."
  (interactive)
  (mpris-call-player "Next")
  (message "Starting next song on %s... %s" mpris-player-name
	   (mpris-song-info-message)))


(defun emp-prev ()
  "Previous song on MPRIS Player."
  (interactive)
  (mpris-call-player "Previous")
  (message "Starting previous song on %s... %s" mpris-player-name
	   (mpris-song-info-message)))

(defun emp-stop ()
  "Stop MPRIS Player."
  (interactive)
  (mpris-call-player "Stop")
  (message "Stopping %s..." mpris-player-name))


(defun emp-volume-up ()
  "Increase volume of MPRIS Player."
  (interactive)
  (let* ((vol (car (mpris-get-prop "Volume")))
	 (newvol (+ vol .1)))
    (mpris-set-prop "Volume" (min newvol 1.0))
    (message "Volume up to %0.2f%%..." (* 100 newvol))))

(defun emp-volume-down ()
  "Decrease volume of MPRIS Player."
  (interactive)
  (let* ((vol (car (mpris-get-prop "Volume")))
	 (newvol (- vol .1)))
    (mpris-set-prop "Volume" (max newvol 0.0))
    (message "Volume down to %0.2f%%..." (* 100 newvol))))

(defun emp-toggle-shuffle ()
  "Toggle whether the current playlist of MPRIS player shuffles"
  (interactive)
  (let ((shuffle? (car (mpris-get-prop "Shuffle"))))
    (mpris-set-prop "Shuffle" (not shuffle?))
    (if shuffle?
	(message "Shuffle turned off")
      (message "Shuffle turned on"))))

(defun emp-status ()
  "Print the status message"
  (interactive)
  (message (mpris-song-info-message)))

(provide 'emp)

;;; emp.el ends here

