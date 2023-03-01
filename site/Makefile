lispdir = lisp
pubdir = public
cachedir = .cache

all: build

build:
	rm -rf $(cachedir)
	emacs --quick --batch --load $(lispdir)/op-publish.el --funcall org-publish-all

pdf: $(pubdir)/darkman.tex | build
	latexmk -silent -pdfxe -interaction=nonstopmode -output-directory=$(<D) $(<F)
	cd $(<D) && latexmk -silent -c

serve: build
	miniserve $(pubdir) --route-prefix=darkman.el --index=index.html

clean:
	git clean -Xdf

.SILENT:
