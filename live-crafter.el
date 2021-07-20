;; -*- lexical-binding: t; -*-

;;; Code:

(require 'request)
(require 'subr-x)
(require 'simple-httpd)
(require 'a)
(require 'mpv)

(defvar live-crafter-youtube-client-id nil)
(defvar live-crafter-youtube-api-key nil)
(defvar live-crafter--auth-redirect-uri "http://localhost:3000/oauth2_callback")
(defvar live-crafter--access-token-func nil)
(defvar live-crafter--use-bearer-auth nil)
(defvar live-crafter--request-url nil)

(defun live-crafter--build-params (params)
  (string-join (mapcar (lambda (pair)
                         (format "%s=%s" (car pair) (cdr pair)))
                       params)
               "&"))

(defun live-crafter--build-uri (path params)
  (let ((param-string (live-crafter--build-params params)))
    (concat path
            (if (> (length param-string) 0) "?" "")
            param-string)))

(defun live-crafter--get-access-token ()
  (funcall live-crafter--access-token-func))

(defun live-crafter--build-auth-url (client-id redirect-uri)
  (live-crafter--build-uri
   "https://accounts.google.com/o/oauth2/v2/auth"
   `((client_id . ,client-id)
     (response_type . "code")
     (redirect_uri . ,redirect-uri)
     ("scope" . ,(string-join '("https://www.googleapis.com/auth/youtube"
                                "https://www.googleapis.com/auth/youtube.force-ssl"
                                "https://www.googleapis.com/auth/youtube.readonly")
                              " ")))))

(defun live-crafter--extract-token (json-body)
  (cdr (assoc 'access_token json-body)))

(defun live-crafter--request-token (auth-code)
  (let* ((params `((client_id . ,live-crafter-youtube-client-id)
                   (client_secret . ,(test--get-client-secret))
                   (code . ,auth-code)
                   (grant_type . "authorization_code")
                   (redirect_uri . ,live-crafter--auth-redirect-uri)))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (live-crafter--build-params params)))
    (with-temp-buffer
      (url-retrieve
       "https://oauth2.googleapis.com/token"
       (lambda (status)
         (re-search-forward "^\n")
         (let ((token-details (json-read)))
           (setq live-crafter--access-token-func
                 #'(lambda ()
                     (live-crafter--extract-token token-details)))))
       nil
       t))))

(defun live-crafter-authenticate ()
  (interactive)
  (let ((httpd-port 3000)
        (auth-url (live-crafter--build-auth-url live-crafter-youtube-client-id
                                                "http://localhost:3000/oauth2_callback")))
    (httpd-start)
    (browse-url auth-url)))

(defun live-crafter-get-subscriber-count ()
  (let ((params `((part . "statistics")
                  (mine . "true")
                  (key . ,live-crafter-youtube-api-key))))
    (live-crafter--http-get
     "https://youtube.googleapis.com/youtube/v3/channels"
     params
     (lambda (response)
       (message "Got response! %S" response)))))

(defvar live-crafter--current-stream nil)

(defun live-crafter-get-current-stream ()
  (live-crafter--http-get
   "https://www.googleapis.com/youtube/v3/liveBroadcasts"
   `((part . "status,snippet")
     (broadcastStatus . "active")
     (key . ,live-crafter-youtube-api-key))
   (lambda (response)
     (let ((old-stream live-crafter--current-stream)
           (items (cdr-safe (assoc 'items response))))
       (setq live-crafter--current-stream
             (when (> (length items) 0)
               (aref items 0)))))))

;; IDEAS:
;; - live-crafter-copy-stream-key
;; - live-crafter-schedule-stream
;; - live-crafter-stream-go-live: POST https://www.googleapis.com/youtube/v3/liveBroadcasts/transition
;; - live-crafter-end-stream

(defun live-crafter-stream-go-live ()
  ;; Steps:
  ;; - Tell OBS to start streaming
  ;; - Monitor the status of the stream to check for when it becomes 'active'
  ;; - When stream is active, change broadcast status to 'live' and kick off intro
  )

(defun live-crafter-get-stream ()
  (live-crafter--http-get
   "https://www.googleapis.com/youtube/v3/liveStreams"
   `((part . "status,snippet")
     (broadcastStatus . "active")
     (key . ,live-crafter-youtube-api-key))
   (lambda (response)
     (let ((old-stream live-crafter--current-stream)
           (items (cdr-safe (assoc 'items response))))
       (setq live-crafter--current-stream
             (when (> (length items) 0)
               (aref items 0)))))))

;; 5 points per request
;; 0.5 requests per second = 30 requests per hour
;; 2 hours
;; = (* 5 30 60 2) = 18000
;; = (* 5 20 60 2) = 12000
;; = (* 5 12 60 2) = 7200

(defvar live-crafter-chat-buffer-name "*stream-chat*")
(defvar live-crafter-chat-polling-interval 5000)
(defvar live-crafter--chat-polling-active nil)

(defvar live-crafter-live-chat-special-users
  '(("UCpNOwhoJbw3q2oJuHU0s6Hg" . vip)
    ("UCAiiOTio8Yu69c3XnR7nQBQ" . channel)
    ("UCkGy7BzQsjmBMTd-scJn0TQUCpNOwhoJbw3q2oJuHU0s6Hg" . sponsor)
    ("UC3NLMkxj-BZZq9vDlp6xfUQ" . sponsor)))

(defun live-crafter--get-live-chat-id ()
  (a-get-in live-crafter--current-stream '(snippet liveChatId)))

(defun live-crafter--poll-chat-messages (live-chat-id on-message-func &optional last-page-token)
  ;; TODO: Don't poll if stream is dead
  (when live-crafter--chat-polling-active
    ;; (message "Page token is: %s" (or last-page-token 'null))
    (let ((params `((part . "snippet,authorDetails")
                    (liveChatId . ,live-chat-id)
                    ;; (pageToken . ,(or last-page-token 'null))
                    (key . ,live-crafter-youtube-api-key))))
      (live-crafter--http-get
       "https://www.googleapis.com/youtube/v3/liveChat/messages"
       params
       (lambda (response)
         ;; Check for error code first
         (let ((error-details (a-get response 'error)))
           (if error-details
               (message "Chat poll failed (code %s): %s"
                        (a-get error-details 'code)
                        (a-get error-details 'message))
             ;; (message "Chat response: %s" (pp-to-string response))
             (let ((polling-interval (max 2000 ;; Request every 2 seconds to avoid wasting quota
                                          (cdr-safe (assoc 'pollingIntervalMillis response))))
                   (next-page-token )
                   (items (cdr-safe (assoc 'items response))))
               ;; (message "Polling in %s secs!" (/ polling-interval 1000.0))
               (mapcar (lambda (message)
                         (funcall on-message-func message))
                       items)
               (run-at-time (/ polling-interval 1000.0)
                            nil
                            #'live-crafter--poll-chat-messages
                            live-chat-id
                            on-message-func
                            (a-get response 'nextPageToken))))))))))

(defun live-crafter--live-chat-style-user (id name)
  (let ((user-type (cdr-safe (assoc id live-crafter-live-chat-special-users)))
        (lock-face 'font-lock-function-name-face))
    (pcase user-type
      ('channel (setq lock-face 'font-lock-variable-name-face))
      ('sponsor (setq lock-face 'font-lock-string-face))
      ('vip (setq lock-face 'font-lock-constant-face)))
    (funcall 'propertize name 'face (list lock-face 'bold))))

(defun live-crafter--on-chat-message (message)
  (with-current-buffer live-crafter-chat-buffer-name
    (let ((inhibit-read-only t))
      (end-of-buffer)
      (newline)
      ;; (message "Text: %s" (a-get-in message '(snippet textMessageDetails messageText)))
      (insert (format "%s: %s"
                      (live-crafter--live-chat-style-user
                       (a-get-in message '(authorDetails channelId))
                       (a-get-in message '(authorDetails displayName)))
                      (a-get-in message '(snippet textMessageDetails messageText)))))))

(defun live-crafter-stream-chat-messages ()
  (interactive)
  (let ((chat-buffer (get-buffer-create live-crafter-chat-buffer-name)))
    (setq live-crafter--chat-polling-active t)
    (live-crafter--poll-chat-messages (live-crafter--get-live-chat-id)
                                      #'live-crafter--on-chat-message)))

(defservlet* oauth2_callback text/plain (code error)
  (if error
      (message "Error during YouTube authentication: %s" error)
    (live-crafter--request-token code))
  (httpd-stop))

(defun live-crafter--http-get (url params callback)
  (let* ((live-crafter--request-url (live-crafter--build-uri url params))
         (url-request-extra-headers
          (append
           `(("Authorization" . ,(format "Bearer %s" (live-crafter--get-access-token))))
           url-request-extra-headers)))
    (with-temp-buffer
      (url-retrieve
       live-crafter--request-url
       (lambda (status)
         (re-search-forward "^\n")
         (funcall callback (json-read)))
       nil
       t))))

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

(defvar live-crafter-before-stream-start-hook nil
  "A hook that runs just before the stream is started.")

(defvar live-crafter-after-stream-start-hook nil
  "A hook that runs just after the stream is started.")

(defun live-crafter-start-stream ()
  (interactive)
  (obs-websocket-send "SetCurrentScene" :scene-name "Logo Screen")
  (obs-websocket-send "StartStreaming")
  (live-crafter-start-music))

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

(provide 'live-crafter)
