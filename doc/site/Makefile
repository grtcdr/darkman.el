all: build

build:
	@rm -rf .cache/
	@emacs --quick --batch --load lisp/op-publish.el --funcall org-publish-all t t

serve: build
	@miniserve public/ --route-prefix=darkman.el --index=index.html

clean:
	@rm -rvf public/
	@rm -rvf .cache/
