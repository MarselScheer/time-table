(define-minor-mode time-table-mode
  "Track times for projects and task in a table"
  :global t
  :init-value nil
  :lighter " tt")

(defcustom time-table-work-hours 8
  "The number of hours for a work day. Will be added to each entry for the time table"
  :type 'integer)

(defun time-table--now-time-stamp()
  "Just returns the current time as yyyy-mm-dd HH:MM:ss"
  (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))

(cl-defun time-table--build-entry (project-name task-name)
  (format "%s,%s,%s,%s" (time-table--now-time-stamp) time-table-work-hours project-name task-name))
