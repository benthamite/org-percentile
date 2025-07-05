;;; percentile-feedback.el --- Live percentile‑feedback in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Your Name
;; SPDX-License-Identifier: MIT
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;;; Commentary:

;; Implements Seth Roberts’ “percentile feedback” productivity metric.
;; Load the file, then enable `percentile-feedback-mode`.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)

;;;; User options

(defgroup percentile-feedback nil
  "Live percentile feedback graph and mode-line indicator."
  :group 'productivity)

(defcustom pf-data-file (expand-file-name "pf-data.el" user-emacs-directory)
  "File where percentile-feedback history is stored."
  :type 'file)

(defcustom pf-org-files nil
  "Optional list of Org files whose CLOCK entries are imported."
  :type '(repeat file))

(defcustom pf-midnight-shift org-extend-today-until
  "Seconds to add to real midnight to define the start of your day.
For example, 21600 means 06:00 am acts as ‘midnight’ for percentile feedback."
  :type 'integer)

(defcustom pf-auto-save-interval 300
  "Seconds between automatic writes of `pf--history` to `pf-data-file`."
  :type 'integer)

;;;; Internal data structures

;; Hash table: date‑string “YYYY‑MM‑DD” ⇒ list of (START END)
(defvar pf--history (make-hash-table :test #'equal))
(defvar pf--mode-line-string "")
(defvar pf--save-timer nil)
(defvar pf--update-timer nil)

;;;; Utility helpers

(defun pf--date-string (&optional time)
  "Return date string in YYYY-MM-DD format.
If TIME is nil, use the current time."
  (let ((time (decode-time (or time (current-time)))))
    (format "%04d-%02d-%02d" (nth 5 time) (nth 4 time) (nth 3 time))))

(defun pf--seconds-since-shifted-midnight (&optional time)
  "Seconds since custom midnight today (may be >24 h if before shift).
If TIME is nil, use the current time."
  (let* ((dec (decode-time (or time (current-time))))
         (sec (+ (* (nth 2 dec) 3600)
                 (* (nth 1 dec) 60)
                 (nth 0 dec)))
         (s (+ sec (- 86400 pf-midnight-shift)))) ; handle negative wrap
    (mod s 86400)))

(defun pf--maybe-load-history ()
  "Load `pf--history` from `pf-data-file` if it exists and is empty."
  (when (and (file-exists-p pf-data-file)
             (eq 0 (hash-table-count pf--history)))
    (with-temp-buffer
      (insert-file-contents pf-data-file)
      (setq pf--history (read (current-buffer))))))

(defun pf--save-history ()
  "Persist `pf--history` to `pf-data-file`."
  (with-temp-file pf-data-file
    (prin1 pf--history (current-buffer))))

;;;; Logging helpers

(defun pf--ensure-day (date)
  "Ensure that DATE exists in `pf--history'."
  (or (gethash date pf--history)
      (puthash date nil pf--history)))

(defun pf--log-period (start end)
  "Add a period [START, END) given as float seconds since shifted midnight."
  (when (< end start) (cl-rotatef start end))
  (let* ((date (pf--date-string))
         (lst  (pf--ensure-day date)))
    (puthash date (cons (cons start end) lst) pf--history)))

;;;###autoload
(defun pf-log-period (seconds &optional end-time)
  "Interactive helper: log SECONDS of work ending at END-TIME (default now)."
  (interactive "nSeconds of work: ")
  (let* ((now   (or end-time (current-time)))
         (now-s (pf--seconds-since-shifted-midnight now))
         (start (- now-s seconds)))
    (pf--log-period start now-s)
    (message "Percentile‑feedback: added period %.0fs" seconds)))

;;;; Org import

(defun pf-import-org-clock-entry (beg end)
  "Parse Org CLOCK entry from BEG to END and add it to `pf--history'."
  (let* ((raw (buffer-substring-no-properties beg end))
         (regexp
          (rx "[" (group (+ digit) "-" (+ digit) "-" (+ digit) " " (+ alpha)
                         " " (+ digit) ":" (+ digit)) "]")))
    (when (string-match regexp raw)
      (let* ((start-s (org-time-string-to-seconds (match-string 1 raw)))
             (end-s   (if (string-match regexp raw (match-end 0))
                          (org-time-string-to-seconds (match-string 1 (substring raw (match-end 0))))
                        (float-time (current-time))))
             (start   (pf--seconds-since-shifted-midnight (seconds-to-time start-s)))
             (end     (pf--seconds-since-shifted-midnight (seconds-to-time end-s)))
             (date    (pf--date-string (seconds-to-time start-s))))
        (puthash date (cons (cons start end) (pf--ensure-day date))
                 pf--history)))))

;;;###autoload
(defun pf-import-org-files (&optional files)
  "Parse CLOCK lines from FILES (default `pf-org-files`) into the history."
  (interactive)
  (dolist (f (or files pf-org-files))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (while (re-search-forward org-clock-line-re nil t)
          (pf-import-org-clock-entry (match-beginning 0) (match-end 0))))))
  (message "Percentile‑feedback: Org import done."))

;;;; Percentile computation

(defun pf--cumulative-today (&optional now)
  "Cumulative seconds for today (shifted‑midnight reference).
If NOW is provided, use it as the current time. Otherwise, use `current-time'."
  (let* ((date  (pf--date-string now))
         (lst   (gethash date pf--history))
         (now-s (pf--seconds-since-shifted-midnight now))
         (sum   0))
    (dolist (pr lst sum)
      (cl-incf sum (max 0 (- (min now-s (cdr pr)) (car pr)))))))

(defun pf--cumul-until (date time-sec)
  "Cumulative seconds for DATE up to TIME-SEC (shifted‑midnight reference)."
  (let ((lst (gethash date pf--history))
        (sum 0))
    (dolist (pr lst sum)
      (cl-incf sum (max 0 (- (min time-sec (cdr pr)) (car pr)))))))

(defun pf-current-percentile (&optional now)
  "Return current percentile as float or nil if no history.
If NOW is provided, use it as the current time. Otherwise, use `current-time'."
  (let* ((now     (or now (current-time)))
         (today   (pf--date-string now))
         (now-sec (pf--seconds-since-shifted-midnight now))
         (today-v (pf--cumulative-today now))
         (scores  '()))
    (maphash
     (lambda (k _v)
       (unless (equal k today)
         (push (pf--cumul-until k now-sec) scores)))
     pf--history)
    (if (null scores)
        100.0
      (let* ((beats (cl-count-if (lambda (x) (<= x today-v)) scores))
             (percent (* 100.0 (/ beats (float (length scores))))))
        percent))))

;;;; Mode‑line & timers

(defun pf--update-mode-line ()
  "Update the mode-line string with the current percentile."
  (setq pf--mode-line-string
        (let* ((p (pf-current-percentile)))
          (if p (format " PF:%2.0f%%" p) " PF:--")))
  (force-mode-line-update))

;;;###autoload
(define-minor-mode percentile-feedback-mode
  "Global minor mode that shows live percentile feedback."
  :global t
  (if percentile-feedback-mode
      (progn
        (pf--maybe-load-history)
        (setq pf--update-timer
              (run-at-time 0 60 #'pf--update-mode-line))
        (setq pf--save-timer
              (run-at-time pf-auto-save-interval
                           pf-auto-save-interval
                           #'pf--save-history))
        (add-to-list 'global-mode-string '(:eval pf--mode-line-string) t))
    ;; disable
    (when pf--update-timer  (cancel-timer pf--update-timer))
    (when pf--save-timer    (cancel-timer pf--save-timer))
    (setq global-mode-string
          (remove '(:eval pf--mode-line-string) global-mode-string))))

(provide 'percentile-feedback)
;;; percentile-feedback.el ends here
