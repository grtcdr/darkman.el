emacs = emacs --batch --directory=.

test: FORCE
	$(emacs) -l test/darkman-test.el -f ert-run-tests-batch-and-exit

clean:
	git clean -Xdf

FORCE:
