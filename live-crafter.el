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

(defun live-crafter-start-recording ()
  (interactive)
  (obs-websocket-send "SetCurrentScene" :scene-name "Logo Screen")
  (run-at-time 1 nil (lambda ()
                       (obs-websocket-send "StartRecording")
                       (live-crafter-timestamps-start)
                       (live-crafter-add-timestamp "Intro")
                       (live-crafter-start-music))))

(defun live-crafter-intro-to-screen ()
  (interactive)
  (live-crafter-fade-music 22 2 (lambda ()
                                  (obs-websocket-send "SetCurrentScene" :scene-name "Screen"))))

(defvar live-crafter--timestamp-start-time nil)
(defvar live-crafter--timestamp-buffer nil)

(defun live-crafter-timestamps-start ()
  (setq live-crafter--timestamp-buffer (find-file-noselect "~/Notes/Streams/Timestamps.org"))
  (setq live-crafter--timestamp-start-time (float-time))
  (let ((inhibit-message t))
    (with-current-buffer live-crafter--timestamp-buffer
      (delete-region (point-min) (point-max))
      (save-buffer))))

(defun live-crafter--format-timestamp (title)
  (format "- %s %s"
          (format-seconds "%h:%z%.2m:%.2s" (- (float-time)
                                            live-crafter--timestamp-start-time))
          title))

(defun live-crafter-add-timestamp (title)
  (when live-crafter--timestamp-start-time
    (let ((inhibit-message t))
      (with-current-buffer live-crafter--timestamp-buffer
        (goto-char (point-max))
        (insert (live-crafter--format-timestamp title))
        (newline)
        (save-buffer)))))

;; TODO: Change scene after finishing fade
;; (live-crafter-fade-music 22.0 2 (lambda () (message "Fade is done!")))
