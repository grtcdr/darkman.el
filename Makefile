test:
	@emacs --batch --eval "(add-to-list 'load-path default-directory)" -l darkman-tests.el -f ert-run-tests-batch-and-exit
