;;; org-percentile.el --- Live percentile feedback from Org clocks -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Pablo Stafforini
;; SPDX-License-Identifier: MIT
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;;; Commentary:

;; Implements Seth Roberts’ “percentile feedback” productivity metric.
;; Load the file, then enable `org-percentile-mode'.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)

;;;; User options

(defgroup org-percentile nil
  "Live percentile feedback from Org clocks."
  :group 'org-clock)

(defcustom org-percentile-data-file (expand-file-name "org-percentile-data.el" user-emacs-directory)
  "File where org-percentile history is stored."
  :type 'file)

(defcustom org-percentile-org-files nil
  "Optional list of Org files whose CLOCK entries are imported."
  :type '(repeat file))

(defcustom org-percentile-midnight-shift org-extend-today-until
  "Seconds to add to real midnight to define the start of your day.
For example, 21600 means 06:00 am acts as ‘midnight’ for percentile feedback."
  :type 'integer)

;;;; Internal data structures

;; Hash table: date‑string “YYYY‑MM‑DD” ⇒ list of (START END)
(defvar org-percentile--history (make-hash-table :test #'equal))
(defvar org-percentile--mode-line-string "")

;;;; Utility helpers

(defun org-percentile--date-string (&optional time)
  "Return date string in YYYY-MM-DD format.
If TIME is nil, use the current time."
  (let ((time (decode-time (or time (current-time)))))
    (format "%04d-%02d-%02d" (nth 5 time) (nth 4 time) (nth 3 time))))

(defun org-percentile--seconds-since-shifted-midnight (&optional time)
  "Seconds since custom midnight today (may be >24 h if before shift).
If TIME is nil, use the current time."
  (let* ((dec (decode-time (or time (current-time))))
         (sec (+ (* (nth 2 dec) 3600)
                 (* (nth 1 dec) 60)
                 (nth 0 dec)))
         (s (+ sec (- 86400 org-percentile-midnight-shift)))) ; handle negative wrap
    (mod s 86400)))

(defun org-percentile--maybe-load-history ()
  "Load `org-percentile--history' from `org-percentile-data-file' if it exists and is empty."
  (when (and (file-exists-p org-percentile-data-file)
             (eq 0 (hash-table-count org-percentile--history)))
    (with-temp-buffer
      (insert-file-contents org-percentile-data-file)
      (setq org-percentile--history (read (current-buffer))))))

(defun org-percentile--save-history ()
  "Persist `org-percentile--history' to `org-percentile-data-file'."
  (with-temp-file org-percentile-data-file
    (prin1 org-percentile--history (current-buffer))))

;;;; Logging helpers

(defun org-percentile--ensure-day (date)
  "Ensure that DATE exists in `org-percentile--history'."
  (or (gethash date org-percentile--history)
      (puthash date nil org-percentile--history)))

(defun org-percentile--log-period (start end date)
  "Add a period [START, END) to DATE, given as float seconds since shifted midnight."
  (when (< end start) (cl-rotatef start end))
  (let ((lst (org-percentile--ensure-day date)))
    (puthash date (cons (cons start end) lst) org-percentile--history)))

(defun org-percentile--log-period-absolute (start-s end-s)
  "Log a period from START-S to END-S, given as absolute seconds since epoch."
  (when (< end-s start-s) (cl-rotatef start-s end-s))
  (let* ((start-time (seconds-to-time start-s))
         (end-time (seconds-to-time end-s))
         (start-date (org-percentile--date-string start-time))
         (end-date (org-percentile--date-string end-time))
         (start-sec-day (org-percentile--seconds-since-shifted-midnight start-time))
         (end-sec-day (org-percentile--seconds-since-shifted-midnight end-time)))
    (if (equal start-date end-date)
        (org-percentile--log-period start-sec-day end-sec-day start-date)
      ;; Period spans across midnight(s)
      ;; 1. Log from start time to end of first day
      (org-percentile--log-period start-sec-day 86400.0 start-date)
      ;; 2. Log full intermediate days
      (let ((current-s (time-add start-s 86400))) ; approx next day
        (while (string< (org-percentile--date-string (seconds-to-time current-s)) end-date)
          (let ((date (org-percentile--date-string (seconds-to-time current-s))))
            (org-percentile--log-period 0.0 86400.0 date))
          (setq current-s (time-add current-s 86400))))
      ;; 3. Log from start of last day to end time
      (org-percentile--log-period 0.0 end-sec-day end-date))))

;;;; Org import

(defun org-percentile-import-org-clock-entry (beg end)
  "Parse Org CLOCK entry from BEG to END and add it to the history."
  (let ((raw (buffer-substring-no-properties beg end)))
    (when (string-match (rx "CLOCK: "
                            (group (in "[<") (+? anything) (in "]>"))
                            "--"
                            (group (in "[<") (+? anything) (in "]>")))
                        raw)
      (let* ((start-str (match-string 1 raw))
             (end-str (match-string 2 raw))
             (start-s (org-time-string-to-seconds start-str))
             (end-s (org-time-string-to-seconds end-str)))
        (org-percentile--log-period-absolute start-s end-s)))))

;;;###autoload
(defun org-percentile-import-org-files (&optional files)
  "Parse CLOCK lines from FILES (default `org-percentile-org-files') into the history."
  (interactive)
  (dolist (f (or files org-percentile-org-files))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (while (re-search-forward (rx bol (0+ (in " \t")) "CLOCK: ") nil t)
          (org-percentile-import-org-clock-entry (line-beginning-position) (line-end-position))))))
  (message "Org Percentile: Org import done."))

;;;; Percentile computation

(defun org-percentile--cumulative-today (&optional now)
  "Cumulative seconds for today (shifted‑midnight reference).
If NOW is provided, use it as the current time. Otherwise, use `current-time'."
  (let* ((date  (org-percentile--date-string now))
         (lst   (gethash date org-percentile--history))
         (now-s (org-percentile--seconds-since-shifted-midnight now))
         (sum   0))
    (dolist (pr lst sum)
      (cl-incf sum (max 0 (- (min now-s (cdr pr)) (car pr)))))))

(defun org-percentile--cumulative-until (date time-sec)
  "Cumulative seconds for DATE up to TIME-SEC (shifted‑midnight reference)."
  (let ((lst (gethash date org-percentile--history))
        (sum 0))
    (dolist (pr lst sum)
      (cl-incf sum (max 0 (- (min time-sec (cdr pr)) (car pr)))))))

(defun org-percentile-current-percentile (&optional now)
  "Return current percentile as float or nil if no history.
If NOW is provided, use it as the current time. Otherwise, use `current-time'."
  (let* ((now     (or now (current-time)))
         (today   (org-percentile--date-string now))
         (now-sec (org-percentile--seconds-since-shifted-midnight now))
         (today-v (org-percentile--cumulative-today now))
         (scores  '()))
    (maphash
     (lambda (k _v)
       (unless (equal k today)
         (push (org-percentile--cumulative-until k now-sec) scores)))
     org-percentile--history)
    (if (null scores)
        100.0
      (let* ((beats (cl-count-if (lambda (x) (<= x today-v)) scores))
             (percent (* 100.0 (/ beats (float (length scores))))))
        percent))))

;;;; Mode‑line & hooks

(defun org-percentile--update-mode-line ()
  "Update the mode-line string with the current percentile."
  (setq org-percentile--mode-line-string
        (let* ((p (org-percentile-current-percentile)))
          (if p (format " PF:%2.0f%%" p) " PF:--")))
  (force-mode-line-update))

(defun org-percentile-clock-in-hook ()
  "Update mode-line when clocking in."
  (org-percentile--update-mode-line))

(defun org-percentile-clock-out-hook ()
  "Record the completed clock period and update display."
  (save-excursion
    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (when (string-match (rx "CLOCK: "
                              (group (in "[<") (+? anything) (in "]>"))
                              "--"
                              (group (in "[<") (+? anything) (in "]>")))
                          line)
        (let* ((start-str (match-string 1 line))
               (end-str (match-string 2 line))
               (start-s (org-time-string-to-seconds start-str))
               (end-s (org-time-string-to-seconds end-str)))
          (org-percentile--log-period-absolute start-s end-s)))))
  (org-percentile--save-history)
  (org-percentile--update-mode-line)
  (message "PF:%2.0f%%" (org-percentile-current-percentile)))

;;;###autoload
(define-minor-mode org-percentile-mode
  "Global minor mode that shows live percentile feedback from Org clocks."
  :global t
  (if org-percentile-mode
      (progn
        (org-percentile--maybe-load-history)
        (add-hook 'org-clock-in-hook #'org-percentile-clock-in-hook)
        (add-hook 'org-clock-out-hook #'org-percentile-clock-out-hook)
        (add-to-list 'global-mode-string '(:eval org-percentile--mode-line-string) t)
        (org-percentile--update-mode-line))
    ;; disable
    (remove-hook 'org-clock-in-hook #'org-percentile-clock-in-hook)
    (remove-hook 'org-clock-out-hook #'org-percentile-clock-out-hook)
    (setq global-mode-string
          (remove '(:eval org-percentile--mode-line-string) global-mode-string))))

(provide 'org-percentile)
;;; org-percentile.el ends here
