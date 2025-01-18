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
