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
    (time-table--prepend-to-buffer "prj" "tsk" "fake-time" buf)
    (setq time-table-work-hours 4)
    (time-table--prepend-to-buffer "prj2" "tsk2" "fake-time2" buf)
    (set-buffer buf)
    (should (string=
	     (buffer-string)
	     "fake-time2,4,prj2,tsk2\nfake-time,8,prj,tsk\n"))))

(ert-deftest time-table--summarize-project-times ()
  "Sum project time by project"
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer)))
    (with-current-buffer buf
      (insert "2015-01-12 14:50:00,8,end,t1\n")
      (insert "2015-01-12 14:30:00,8,end,t1\n")
      (insert "2015-01-12 14:00:00,8,p2,t1\n")
      (insert "2015-01-12 13:00:00,8,p1,t1\n")
      (insert "2015-01-12 12:00:00,8,p1,t1")
      )
    (set-buffer "*scratch*")
    (should (string=
	   (format "%s" (time-table--summarize-project-times buf))
	   (format "%s" (list (list "p1" 2.0) (list "p2" 0.5)))))))

(ert-deftest time-table--summarize-project-time-uses-implicit-end-task ()
  "Check that time-table entries are prepended to buffer"
  (let* (
	 (buf (ms/fixture-time-table-empty-buffer)))
    (time-table--prepend-to-buffer "prj" "tsk" (ms/now 3600) buf)
    (set-buffer "*scratch*")
    (should (string=
	   (format "%s" (time-table--summarize-project-times buf))
	   (format "%s" (list (list "prj" 1.0)))))))
