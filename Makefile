test: FORCE
	@emacs --batch --eval "(add-to-list 'load-path default-directory)" -l test/darkman-test.el -f ert-run-tests-batch-and-exit

FORCE:
