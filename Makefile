SHELL := /bin/bash
.ONESHELL:

no-target:
	@echo -------------------- $@ $$(date) --------------------
	echo No target specified

unit-tests:
	@echo -------------------- $@ $$(date) --------------------
	emacs -batch -l ert -l time-table.el -l tests/test-time-table.el -f ert-run-tests-batch-and-exit
