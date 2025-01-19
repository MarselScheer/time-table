(defun ms/time-stamp-to-sec (ts)
  (time-convert (date-to-time ts) 'integer))

(defun ms/time-stamp-nearly-same-p (ts1 ts2)
  (let (
	(a (ms/time-stamp-to-sec ts1))
	(b (ms/time-stamp-to-sec ts2)))
    (< (abs (- a b)) 2)))

(defun ms/now ()
  (format-time-string
   "%Y-%m-%d %H:%M:%S" (current-time)))

(defun ms/fixture-time-table-entry()
  (time-table--build-entry "proj" "task"))

(defun ms/fixture-time-table-empty-buffer ()
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
  "Check that all entries build are correct"
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
    (sleep-for 1)
    (setq time-table-work-hours 4)
    (time-table--prepend-to-buffer "prj2" "tsk2" "fake-time2" buf)
    (set-buffer buf)
    (should (string=
	     (buffer-string)
	     "fake-time2,4,prj2,tsk2\nfake-time,8,prj,tsk\n"))))
