(define-minor-mode time-table-mode
  "Track times for projects and task in a table"
  :global t
  :init-value nil
  :lighter " tt")

(defcustom time-table-project-names '("Project 1" "Project 2" "Project 3", "end")
  "List of available project names"
  :type 'list)

(defcustom time-table-task-names '("Task 1" "Task 2" "Task 3", "end")
  "List of available task names"
  :type 'list)

(defcustom time-table-work-hours 8
  "The number of hours for a work day. Will be added to each entry for the time table"
  :type 'integer)

(defcustom time-table-file (format "%s/Documents/time-table/tracked-times" (getenv "HOME"))
  "File used to persist tracked times"
  :type 'string)

(defvar time-table-expected-work-time-col 0
  "Column number for the time expected worked in the time-table-structure")

(defvar time-table-actual-work-time-col 1
  "Column number for the time acutally worked in the time-table-structure")

(defvar time-table-time-stamp-col 2
  "Column number for the time stamp in the time-table-structure")

(defvar time-table-project-col 4
  "Column number for the project-name in the time-table-structure")

(defvar time-table-task-col 5
  "Column number for the task-name in the time-table-structure")

(defun time-table--now-time-stamp()
  "Just returns the current time as yyyy-mm-dd HH:MM:ss"
  (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))

(defun time-table--empty-buffer (buf-name)
  (when (get-buffer buf-name)
    (kill-buffer buf-name))
  (generate-new-buffer buf-name))

(defun time-table--empty-file-buffer (buf-name)
  (when (get-buffer buf-name)
    (kill-buffer buf-name))
  (let (
	(rtn (find-file-noselect buf-name)))
    (with-current-buffer rtn
      (erase-buffer))
    rtn))

(cl-defun time-table--build-entry
    (project-name
     task-name
     &optional
     (time-stamp (time-table--now-time-stamp)))
  (format "%s,%s,%s,%s" time-stamp time-table-work-hours project-name task-name))

(cl-defun time-table--prepend-to-buffer
    (project-name
     task-name
     &optional
     _buffer
     (_time-stamp (time-table--now-time-stamp)))
  "Prepends a row like

2015-01-12 14:50:00,8,PROJECT-NAME,TASK-NAME

to _BUFFER"

  (with-current-buffer _buffer
    (goto-line 0)
    (insert "\n")
    (goto-line 0)
    (insert (time-table--build-entry project-name task-name _time-stamp))))

(defun time-table--stamp-to-sec (ts)
  (time-convert (date-to-time ts) 'integer))


(defun time-table--time-stamp-diff (curr-line-data next-line-data)
  (let (
	(a (time-table--stamp-to-sec (nth 0 curr-line-data)))
	(b (time-table--stamp-to-sec (nth 0 next-line-data)))
	(next-project (nth 2 next-line-data)))
    (if (string= next-project "end")
	0
      (- a b))
  ))

(defun time-table--keep-yyyymmdd (time-stamp)
  (nth 0 (string-split time-stamp " ")))




(defun time-table--hours-to-work (curr-line-data next-line-data row very-last-row)
  (let (
	(curr-day (time-table--keep-yyyymmdd (nth 0 curr-line-data)))
	(next-day (time-table--keep-yyyymmdd (nth 0 next-line-data)))
	(expected-hours-to-work (* 3600 (string-to-number (nth 1 curr-line-data))))
	(e nil))
    (if (string= curr-day next-day)
	(if (= (+ 1 row) very-last-row)
	    expected-hours-to-work
	  0)
      expected-hours-to-work)))

(defun time-table--hours-to-work2 (curr-line-data next-line-data)
  (let (
	(curr-day (time-table--keep-yyyymmdd (nth 0 curr-line-data)))
	(next-day (time-table--keep-yyyymmdd (nth 0 next-line-data)))
	(expected-hours-to-work (* 3600 (string-to-number (nth 1 next-line-data)))))
    (if (string= curr-day next-day)
	0
      expected-hours-to-work)))

(defun time-table--raw-data-from-line (line)
  (goto-line line)
  (split-string (thing-at-point 'line) ","))

(defun time-table--to-list (_buffer)
  "Expects that the content of _BUFFER is like

      ...
      2015-01-12 14:00:00,8,p2,taskx
      2015-01-12 13:00:00,8,p1,taskx
      2015-01-12 12:00:00,8,p1,task2
      ...
 

and converts it into a list of lists

     (... (28800 3600 2015-01-12 12:00:00 8 p1 task2)
       (0 3600 2015-01-12 13:00:00 8 p1 taskx)
       (0 1800 2015-01-12 14:00:00 8 p2 taskx) ...)

It reads like follow:

On project p1 we spend 3600 seconds on task2 and we started with task2 at 12.
On that particular day we had to work 8h (28800 sec)

The edge case that _BUFFER is empty nil is returned.

Note the number of seconds we must work on a particular is only once per set
and 0 for the other day. This allows then to calculate easily for each day how
much time is left or we are already over time by simply summing up the 0-th
and 1-th column and substract them.

Also implicitly adds an end task as latest entry in order to also calculate
the time worked for the latest task.
"
  (with-current-buffer _buffer
    (cond ((= (buffer-size _buffer) 0) nil)
	  ((let (
		 (rtn (list))
		 (first-line-data nil)
		 (next-line-data nil)
		 (curr-line-data nil)
		 (last-line-data nil)
		 (current-line 0)
		 (elements nil)
		 (last-line (line-number-at-pos (point-max))))
	     (while (< current-line last-line)
	       (if (= current-line 0)
		   (setq curr-line-data (split-string (time-table--build-entry "end" "end" (time-table--now-time-stamp)) ","))
		 (progn
		   (goto-line current-line)
		   (setq curr-line-data (split-string (thing-at-point 'line) ","))))
	       (goto-line (+ current-line 1))
	       (setq next-line-data (split-string (thing-at-point 'line) ","))
	       (setq elements (split-string (thing-at-point 'line) ","))
	       (setq elements (mapcar 'string-trim elements))
	       (push (time-table--time-stamp-diff curr-line-data next-line-data) elements)
	       ;; (push (time-table--hours-to-work curr-line-data next-line-data current-line last-line) elements)

	       (if (= current-line 0)
		   (push (* 3600 (string-to-number (nth 1 next-line-data))) elements)
		 (push (time-table--hours-to-work2 curr-line-data next-line-data) elements))
	       (push elements rtn)
	       (setq current-line (+ 1 current-line)))
	     rtn)))))

(setq DEBUG t)
(defun time-table--debug-message (info obj)
  (if (eq DEBUG t)
      (progn
	(message (format "%s" info))
	(message (format "%s" obj)))
    nil))

(defun time-table--project-list (time-table-list)
  "Extracts from TIME-TABLE-LIST the set of project-names and returns them
as a list

See `time-table--to-list' for the structure of TIME-TABLE-LIST"
  (let (rtn)
    (dolist (e time-table-list rtn)
      (setq rtn (add-to-list 'rtn (nth time-table-project-col e))))))

(defun time-table--remove-end-project (time-table-list)
  "Remove from TIME-TABLE-LIST every entry where the project-name equals 'end'

See `time-table--to-list' for the structure of TIME-TABLE-LIST"
  (let (rtn)
    (setq rtn (mapcar
	     (lambda(x)
	       (when (not (equal (nth time-table-project-col x) "end")) x))
	     time-table-list))
    (delq nil rtn)))

(defun time-table--filter-project (time-table-list item-str)
  "Keeps TIME-TABLE-LIST only entries where the project-name equals ITEM-STR.

See `time-table--to-list' for the structure of TIME-TABLE-LIST"
  (let (rtn)
    (setq rtn (mapcar
	     (lambda(x)
	       (when (equal (nth time-table-project-col x) item-str) x))
	     time-table-list))
    (delq nil rtn)))

(defun time-table--sum-actual-work-time (a b)
  (+ a (nth time-table-actual-work-time-col b)))

(defun time-table--sum-expected-work-time (a b)
  (+ a (nth time-table-expected-work-time-col b)))

(defun time-table--2-digit-hour (x)
  "Converts seconds to hours with the precision of 2 digits"
  (/ (round (/ x 36.0)) 100.0))

(defun time-table--sum-times-for-project (time-table-list item-str)
  "Calculates  the sum of the hours actually worked on project with name ITEM-STR
using TIME-TABLE-LIST as the basis for the calculations.

Returns the list like ('project-name-x' 3.23)

See `time-table--to-list' for the structure of TIME-TABLE-LIST"
  (let ((sub) (sum_h))
    (setq sub (time-table--filter-project time-table-list item-str))
    (setq sum_h (time-table--2-digit-hour (seq-reduce 'time-table--sum-actual-work-time sub 0)))
    (list item-str sum_h)))


(cl-defun time-table--summarize-project-times (time-table-list)
  "Sums up the time spend per project contained in TIME-TABLE-LIST

It returns a list of lists, like (('project-name1' 2.3) ('project-name2' 0.2))

See `time-table--to-list' for the structure of TIME-TABLE-LIST"
  (let* (
	 (projects (time-table--project-list (time-table--remove-end-project time-table-list))))
    (let (rtn)
      (dolist (e projects rtn)
	(push (time-table--sum-times-for-project time-table-list e) rtn))
      rtn)))


(cl-defun time-table--over-hours (_buffer)
  "Calculates the over hours based on _BUFFER. If _BUFFER is empty it returns 0"
  (let* (
	 (tt-list (time-table--to-list _buffer))
	 (actual-hours 0)
	 (expected-hours 0))

    (cond ((when (eq tt-list nil)) 0)
	  ((setq actual-hours (seq-reduce 'time-table--sum-actual-work-time tt-list 0))
	   (setq expected-hours (seq-reduce 'time-table--sum-expected-work-time tt-list 0))
	   (time-table--2-digit-hour (- actual-hours expected-hours))))))

(cl-defun time-table--keep-last-x-days
    (time-table-list
     &optional
     (num-days 7)
     (_time-stamp (time-table--now-time-stamp)))
  "Keeps only those elements from TIME-TABLE-LIST where their time is not older than NUM_DAYS (default 7) days compared
to 'today 00:00:00'.

_TIME-STAMP is not meant to be used but ease testing.

See `time-table--to-list' for the structure of TIME-TABLE-LIST"
  (let (
	(last-day-to-keep
	 (-
	  (time-convert (date-to-time (format "%s 00:00:00" (time-table--keep-yyyymmdd _time-stamp))) 'integer)
	  (* 3600 24 (- num-days 1)))))
    (seq-keep
     (lambda(x)
       (if
	   (< last-day-to-keep (time-convert (date-to-time (nth time-table-time-stamp-col x)) 'integer))
	   x
	 nil))
     time-table-list)))

(defun time-table--load-track-file ()
  "If file specified by custom var TIME-TABLE-FILE does not exist it creates the necessary folders and the file.
If a buffer to that file is open already it returns that buffer other it opens file in a buffer and returns that buffer."
  (unless (file-exists-p time-table-file)
      (make-directory (file-name-directory time-table-file) t))
  (let ((buffer (get-file-buffer time-table-file)))
    (if buffer
        buffer
      (find-file-noselect time-table-file))))

(defun time-table-over-hours ()
  "Shows the over hours in the minibuffer"
  (interactive)
  (let ((track-buffer (time-table--load-track-file)))
    (message (format "Over hours: %s" (time-table--over-hours track-buffer)))))

(defun time-table-track ()
  "Adds a new entry to the buffer that holds the tracked times. Interactively asking for a project and task to track.

Suggestion for the projects and tasks are defined in the custom vars TIME-TABLE-PROJECT-NAMES and TIME-TABLE-TASK-NAMES
"
  (interactive)
  (let* ((project-name (completing-read "Select a project: " time-table-project-names))
        (task-name (completing-read "Select a task: " time-table-task-names))
	(track-buffer (time-table--load-track-file))
	(over-hours (time-table--over-hours track-buffer)))
    (time-table--prepend-to-buffer project-name task-name track-buffer)
    (with-current-buffer track-buffer
      (save-buffer))
    (message (format "Tracking %s/%s. Over hours: %s" project-name task-name over-hours))))

(defun time-table-summarize-projects-last-7-days ()
  "Show the hours in the last 7 days spend per project in the minibuffer"
  (interactive)
  (let* (
	 (track-buffer (time-table--load-track-file))
	 (time-table-list (time-table--keep-last-x-days (time-table--to-list track-buffer)))
	 (summary (time-table--summarize-project-times time-table-list)))
    (message (format "%s" summary))))
    
(defun time-table--status (buffer)
  "Returns the first row of a BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-line 1)
      (string-trim (thing-at-point 'line)))))

(defun time-table--hours-worked-today (_buffer)
  "Calculate the number of hours worked today"
  (let* (
	 (tt-list (time-table--to-list _buffer))
	 (actual-hours 0))
    (setq tt-list (time-table--keep-last-x-days tt-list 1))
    (cond ((when (eq tt-list nil)) 0)
	  ((setq actual-hours (seq-reduce 'time-table--sum-actual-work-time tt-list 0))
	   (time-table--2-digit-hour actual-hours)))))

(defun time-table-status ()
  "Shows the hours worked, over hours and the project/task currently tracking in the minibuffer"
  (interactive)
  (let ((track-buffer (time-table--load-track-file)))
    (message
     (format
      "Total hours today : %s\nTotal over hours  : %s\nCurrently tracking: %s"
      (time-table--hours-worked-today track-buffer)
      (time-table--over-hours track-buffer)
      (time-table--status track-buffer)))))


(cl-defun time-table--summarize-task-times (buffer)
  "Sums up the time spend per task contained in BUFFER

It returns a list of lists, like (('task-name1' 2.3) ('task-name2' 0.2))"
  (let* (
	 (time-table-list (time-table--to-list buffer))
	 (tasks (time-table--task-list (time-table--remove-end-project time-table-list))))
    (let (rtn)
      (dolist (e tasks rtn)
	(push (time-table--sum-times-for-tasks time-table-list e) rtn))
      rtn)))

(defun time-table--task-list (time-table-list)
  "Extracts from TIME-TABLE-LIST the set of task-names and returns them
as a list

See `time-table--to-list' for the structure of TIME-TABLE-LIST"
  (let (rtn)
    (dolist (e time-table-list rtn)
      (setq rtn (add-to-list 'rtn (nth time-table-task-col e))))))

(defun time-table--sum-times-for-tasks (time-table-list item-str)
  "Calculates  the sum of the hours actually worked on task with name ITEM-STR
using TIME-TABLE-LIST as the basis for the calculations.

Returns the list like ('task-name-x' 3.23)

See `time-table--to-list' for the structure of TIME-TABLE-LIST"
  (let ((sub) (sum_h))
    (setq sub (time-table--filter-tasks time-table-list item-str))
    (setq sum_h (time-table--2-digit-hour (seq-reduce 'time-table--sum-actual-work-time sub 0)))
    (list item-str sum_h)))

(defun time-table--filter-tasks (time-table-list item-str)
  "Keeps TIME-TABLE-LIST only entries where the task-name equals ITEM-STR.

See `time-table--to-list' for the structure of TIME-TABLE-LIST"
  (let (rtn)
    (setq rtn (mapcar
	     (lambda(x)
	       (when (equal (nth time-table-task-col x) item-str) x))
	     time-table-list))
    (delq nil rtn)))

(defun time-table-summarize-tasks ()
  "Show the hours in the last 7 days spend per project in the minibuffer"
  (interactive)
  (let* (
	 (track-buffer (time-table--load-track-file))
	 (summary (time-table--summarize-task-times track-buffer)))
    (message (format "%s" summary))))
