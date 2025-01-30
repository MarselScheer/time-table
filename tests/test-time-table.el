(defun ms/time-stamp-to-sec (ts)
  (time-convert (date-to-time ts) 'integer))

(defun ms/time-stamp-nearly-same-p (ts1 ts2)
  (let (
	(a (ms/time-stamp-to-sec ts1))
	(b (ms/time-stamp-to-sec ts2)))
    (< (abs (- a b)) 2)))

(cl-defun ms/now (&optional (substract 0))
  (format-time-string
   "%Y-%m-%d %H:%M:%S" (- (time-convert (current-time) 'integer) substract)))

(defun ms/fixture-time-table-entry()
  (time-table--build-entry "proj" "task"))

(defun ms/fixture-time-table-empty-buffer ()
  "Creates an empty buffer to more easily test non-interactive
functions. The SUT usually writes to the this empty buffer
and later assertions are made on the buffer content"
  (let (
	(buf-name "unit-test-time-table"))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let (
	  (rtn (generate-new-buffer buf-name)))
      (set-buffer rtn)
      rtn)))

(ert-deftest time-table--build-entry ()
  "Check that all entries build are correct"
  (let (
	(out (ms/fixture-time-table-entry)))
    (should (ms/time-stamp-nearly-same-p
	     (ms/now)
	     (nth 0 (string-split out ","))))
    (should (string=
	     "8"
	     (nth 1 (string-split out ","))))
    (should (string=
	     "proj"
	     (nth 2 (string-split out ","))))
    (should (string=
	     "task"
	     (nth 3 (string-split out ","))))))

(ert-deftest time-table--build-entry-with-user-work-hours ()
  "Check that work hours of user are considered"
  (let* (
	 (time-table-work-hours 4)
	 (out (ms/fixture-time-table-entry)))
    (should (string=
	     "4"
	     (nth 1 (string-split out ","))))))


(ert-deftest time-table--prepend-to-buffer ()
  "Check that time-table entries are prepended to buffer"
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer)))
    (time-table--prepend-to-buffer "prj" "tsk" buf "fake-time")
    (setq time-table-work-hours 4)
    (time-table--prepend-to-buffer "prj2" "tsk2" buf "fake-time2")
    (set-buffer buf)
    (should (string=
	     (buffer-string)
	     "fake-time2,4,prj2,tsk2\nfake-time,8,prj,tsk\n"))))

(ert-deftest time-table--over-hours-of-an-empty-buffer-is-zero ()
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer)))
    (set-buffer "*scratch*")
    (time-table--debug-message "?: " (time-table--over-hours buf))
    (should (=
	     (time-table--over-hours buf)
	     0))))

(ert-deftest time-table--calcs-over-hours-with-implicit-end-task ()
  "Calculates over hours using implicit end task"
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer)))
    (time-table--prepend-to-buffer "prj" "tsk" buf (ms/now 3600))
    (set-buffer "*scratch*")
    (should (=
	     (time-table--over-hours buf)
	     -7.0))))

(ert-deftest time-table--calcs-over-hours-if-expected-hours-change ()
  "Sum project time by project"
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer)))
    (with-current-buffer buf
      (insert "2015-01-13 14:50:00,1,end,t1\n")
      (insert "2015-01-13 14:30:00,1,end,t1\n")
      (insert "2015-01-13 14:00:00,1,p2,t1\n")
      (insert "2015-01-13 13:00:00,1,p1,t1\n")
      (insert "2015-01-12 14:30:00,4,end,t1\n")
      (insert "2015-01-12 14:00:00,4,p2,t1\n")
      (insert "2015-01-12 13:00:00,4,p1,t1\n")
      (insert "2015-01-12 12:00:00,4,p1,t1")
      )
    (set-buffer "*scratch*")
    (should (=
	     (time-table--over-hours buf)
	     (+ (- 2.5 4) (- 1.5 1))))))

(ert-deftest time-table--summarize-project-times ()
  "Sum project time by project"
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer))
	 (tt-list))
    (with-current-buffer buf
      (insert "2015-01-12 14:50:00,8,end,t1\n")
      (insert "2015-01-12 14:30:00,8,end,t1\n")
      (insert "2015-01-12 14:00:00,8,p2,t1\n")
      (insert "2015-01-12 13:00:00,8,p1,t1\n")
      (insert "2015-01-12 12:00:00,8,p1,t1")
      )
    (set-buffer "*scratch*")
    (setq tt-list (time-table--to-list buf))
    (should (string=
	   (format "%s" (time-table--summarize-project-times tt-list))
	   (format "%s" (list (list "p1" 2.0) (list "p2" 0.5)))))))

(ert-deftest time-table--summarize-project-time-uses-implicit-end-task ()
  "Sum project time using implicit end task"
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer))
	 (tt-list))
    (time-table--prepend-to-buffer "prj" "tsk" buf (ms/now 3600))
    (set-buffer "*scratch*")
    (setq tt-list (time-table--to-list buf))
    (should (string=
	   (format "%s" (time-table--summarize-project-times tt-list))
	   (format "%s" (list (list "prj" 1.0)))))))


(ert-deftest time-table--filter-time-table-list-by-timestamp ()
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer))
	 (tt)
	 (filtered-tt))
    (with-current-buffer buf
      (insert "2015-01-15 14:50:00,1,end,t1\n")
      (insert "2015-01-13 14:30:00,1,end,t1\n")
      (insert "2015-01-13 14:00:00,1,p2,t1\n")
      (insert "2015-01-13 13:00:00,1,p1,t1\n")
      (insert "2015-01-12 14:30:00,4,end,t1\n")
      (insert "2015-01-12 14:00:00,4,p2,t1\n")
      (insert "2015-01-12 13:00:00,4,p1,t1\n")
      (insert "2015-01-12 12:00:00,4,p1,t1")
      )
    (set-buffer "*scratch*")
    (setq tt (time-table--to-list buf))
    (setq filtered-tt (time-table--keep-last-x-days tt 7 "2015-01-19 08:09:10"))
    (should (length= filtered-tt 4))
    (should (string=
	     (nth time-table-time-stamp-col (nth 3 filtered-tt))
	     "2015-01-15 14:50:00"))
    (should (string=
	     (nth time-table-time-stamp-col (nth 0 filtered-tt))
	     "2015-01-13 13:00:00"))))

(ert-deftest time-table--status-returns-latest-entry ()
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer)))
    (with-current-buffer buf
      (insert "2015-01-15 14:50:00,1,end,t1\n")
      (insert "2015-01-13 14:50:00,1,whatever,t1\n")
      (insert "2015-01-12 12:00:00,4,p1,t1")
      )
    (set-buffer "*scratch*")
    (should (string=
	     (time-table--status buf)
	     "2015-01-15 14:50:00,1,end,t1"))))

(ert-deftest time-table--hours-worked-today-use-implicit-end ()
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer)))
    (with-current-buffer buf
      (insert "2015-01-13 14:00:00,1,p2,t1\n")
      (insert "2015-01-13 13:00:00,1,p1,t1\n")
      (insert "2015-01-12 14:30:00,4,end,t1\n")
      (insert "2015-01-12 14:00:00,4,p2,t1\n")
      (insert "2015-01-12 13:00:00,4,p1,t1\n")
      (insert "2015-01-12 12:00:00,4,p1,t1")
      )
    (time-table--prepend-to-buffer "oth" "tas" buf (ms/now 1800))
    (time-table--prepend-to-buffer "end" "end" buf (ms/now 3600))
    (time-table--prepend-to-buffer "prj" "tsk" buf (ms/now 5400))
    (set-buffer "*scratch*")
    (should (=
	     (time-table--hours-worked-today buf)
	     1.0))))

(ert-deftest time-table--hours-worked-today-is-zero-if-no-entry-for-today ()
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer)))
    (with-current-buffer buf
      (insert "2015-01-13 14:00:00,1,p2,t1\n")
      (insert "2015-01-13 13:00:00,1,p1,t1\n")
      (insert "2015-01-12 14:30:00,4,end,t1\n")
      (insert "2015-01-12 14:00:00,4,p2,t1\n")
      (insert "2015-01-12 13:00:00,4,p1,t1\n")
      (insert "2015-01-12 12:00:00,4,p1,t1")
      )
    (set-buffer "*scratch*")
    (should (=
	     (time-table--hours-worked-today buf)
	     0.0))))

(ert-deftest time-table--hours-worked-today-zero-if-buffer-is-emtpy ()
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer)))
    (set-buffer "*scratch*")
    (should (=
	     (time-table--hours-worked-today buf)
	     0.0))))

(ert-deftest time-table--summarize-task-times-ignoring-end-projects ()
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer))
	 (tt-list))
    (with-current-buffer buf
      (insert "2015-01-12 14:50:00,8,end,t1\n")
      (insert "2015-01-12 14:30:00,8,end,t1\n")
      (insert "2015-01-12 14:00:00,8,p2,t1\n")
      (insert "2015-01-12 13:00:00,8,p1,t2\n")
      (insert "2015-01-12 12:00:00,8,p1,t1")
      )
    (set-buffer "*scratch*")
    (should (string=
	   (format "%s" (time-table--summarize-task-times buf))
	   (format "%s" (list (list "t1" 1.5) (list "t2" 1.0)))))))

(ert-deftest time-table--summarize-task-times-use-implicit-end ()
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer))
	 (tt-list))
    (with-current-buffer buf
      (insert "2015-01-12 14:50:00,8,end,t1\n")
      (insert "2015-01-12 14:30:00,8,end,t1\n")
      (insert "2015-01-12 14:00:00,8,p2,t1\n")
      (insert "2015-01-12 13:00:00,8,p1,t2\n")
      (insert "2015-01-12 12:00:00,8,p1,t1")
      )
    (time-table--prepend-to-buffer "oth" "tas" buf (ms/now 1800))
    (set-buffer "*scratch*")
    (should (string=
	   (format "%s" (time-table--summarize-task-times buf))
	   (format "%s" (list (list "t1" 1.5) (list "t2" 1.0) (list "tas" 0.5)))))))

(ert-deftest time-table--summarize-task-times-is-nil-if-buffer-empty ()
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer))
	 (tt-list))
    (set-buffer "*scratch*")
    (should (eq
	     (time-table--summarize-task-times buf)
	     nil))))

