EMACS := emacs

all:
	$(EMACS) --batch --quick \
		--directory . \
		--load ~/.emacs.d/elpa/mermaid-mode-*.*/mermaid-mode.el \
		--load mermaid-docker-mode-tests.el \
		--funcall ert-run-tests-batch
