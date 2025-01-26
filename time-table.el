(define-minor-mode time-table-mode
  "Track times for projects and task in a table"
  :global t
  :init-value nil
  :lighter " tt")

(defcustom time-table-work-hours 8
  "The number of hours for a work day. Will be added to each entry for the time table"
  :type 'integer)

(defvar time-table-actual-work-time-col 1
  "Column number for the time acutally worked in the time-table-structure")

(defvar time-table-project-col 4
  "Column number for the project-name in the time-table-structure")

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
     (time-stamp (time-table--now-time-stamp))
     _buffer)
  "Prepends a row like

2015-01-12 14:50:00,8,PROJECT-NAME,TASK-NAME

to _BUFFER"

  (with-current-buffer _buffer
    (goto-line 0)
    (insert "\n")
    (goto-line 0)
    (insert (time-table--build-entry project-name task-name time-stamp))))

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

Note the number of seconds we must work on a particular is only once per set
and 0 for the other day. This allows then to calculate easily for each day how
much time is left or we are already over time by simply summing up the 0-th
and 1-th column and substract them.

Also implicitly adds an end task as latest entry in order to also calculate
the time worked for the latest task.
"
  (with-current-buffer _buffer
    (let (
	  (rtn (list))
	  (next-line-data nil)
	  (curr-line-data nil)
	  (last-line-data nil)
	  (current-line 0)
	  (elements nil)
	  (last-line (line-number-at-pos (point-max))))
      (while (< current-line last-line)
	(time-table--debug-message "ts:" (time-table--now-time-stamp))
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
	(push (time-table--hours-to-work curr-line-data next-line-data current-line last-line) elements)
	(push elements rtn)
	(setq current-line (+ 1 current-line)))
      rtn)))

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
  ;; (time-table--debug-message "filter-in: " time-table-list)
  (let (rtn)
    (setq rtn (mapcar
	     (lambda(x)
	       (when (equal (nth time-table-project-col x) item-str) x))
	     time-table-list))
  ;; (time-table--debug-message "filter-out: " rtn)
    (delq nil rtn)))

(defun time-table--sum-actual-work-time (a b)
  (+ a (nth time-table-actual-work-time-col b)))

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


(cl-defun time-table--summarize-project-times (_buffer)
  "Sums up the time spend per project contained in the time table contained in _BUFFER.

It returns a list of lists, like (('project-name1' 2.3) ('project-name2' 0.2))"
  (let* (
	 (tt-list (time-table--to-list _buffer))
	 (projects (time-table--project-list (time-table--remove-end-project tt-list))))
    (let (rtn)
      (dolist (e projects rtn)
	;; (time-table--debug-message "summary:" rtn)
	(push (time-table--sum-times-for-project tt-list e) rtn))
      ;; (time-table--debug-message "summary:" rtn)
      rtn)))
