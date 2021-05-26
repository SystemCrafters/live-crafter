;; -*- lexical-binding: t; -*-

;;; Code:

(require 'request)
(require 'subr-x)
(require 'simple-httpd)
(require 'a)
(require 'mpv)

(defvar live-crafter-music-start-volume 70.0)
(defvar live-crafter-music-current-volume 70.0)
(defvar live-crafter-music-fade-interval 0.05)
(defvar live-crafter-music-fade-default-duration 2.0)

(defun live-crafter-start-music ()
  (interactive)
  (setq live-crafter-music-current-volume live-crafter-music-start-volume)
  (mpv-start "--playlist=/home/daviwil/Notes/Streams/playlist.txt"
             "--loop-playlist"
             "--no-resume-playback"
             (format "--volume=%s" live-crafter-music-current-volume)))

(defun live-crafter-stop-music ()
  (interactive)
  (mpv-kill))

(defun live-crafter-set-music-volume (volume)
  (setq live-crafter-music-current-volume volume)
  (mpv-run-command "set_property" "volume" volume))

(defun live-crafter--fade-music-step (start-volume start-time target-volume duration callback)
  (let ((time-val (/ (- (float-time) start-time) duration)))
    (live-crafter-set-music-volume
     (if (> time-val 1.0)
         (progn
           (when callback
             (funcall callback))
           target-volume)
       (run-at-time live-crafter-music-fade-interval
                    nil
                    #'live-crafter--fade-music-step
                    start-volume
                    start-time
                    target-volume
                    duration
                    callback)
       ;; Linear interpolation
       (+ (* (- 1 time-val) start-volume)
          (* time-val target-volume))))))

(defun live-crafter-fade-music (target-volume &optional duration callback)
  (interactive "nTarget volume: ")
  (live-crafter--fade-music-step live-crafter-music-current-volume
                                 (float-time)
                                 target-volume
                                 (or duration live-crafter-music-fade-default-duration)
                                 callback))

;; TODO: Change scene after finishing fade
;; (live-crafter-fade-music 35.0 2 (lambda () (message "Fade is done!")))
