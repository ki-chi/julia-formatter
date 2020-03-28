.POXIS:
EMACS = emacs
VERSION = 0.1.0

package: julia-formatter-$(VERSION).tar
julia-formatter-$(VERSION).tar:
	rm -rf julia-formatter-$(VERSION)/
	mkdir julia-formatter-$(VERSION)/
	cp julia-formatter.el julia-formatter-$(VERSION)/
	cp -r scripts julia-formatter-$(VERSION)/
	tar cf $@ julia-formatter-$(VERSION)/
	rm -rf julia-formatter-$(VERSION)/

compile: julia-formatter.elc

test: julia-formatter-test.elc
	$(EMACS) -Q -batch -l ert -l tests/julia-formatter-test.elc -f ert-run-tests-batch-and-exit

clean:
	rm -f julia-formatter-$(VERSION).tar julia-formatter.elc tests/julia-formatter-test.elc

julia-formatter-test.elc: tests/julia-formatter-test.el julia-formatter.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) --batch -Q -L . -f batch-byte-compile $<
