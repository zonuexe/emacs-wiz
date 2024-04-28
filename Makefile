EMACS ?= emacs
CASK ?= cask
EASK ?= eask

compile:
	$(EASK) compile

all: autoloads $(ELCS)

autoloads:
	$(EASK) generate autoloads

.eask: Eask
	$(EASK) install

clean:
	$(EASK) clean all

# Runs all unit tests from php-mode-test.el and shows the results. The
# script will exit with the status code zero if all tests pass. If any
# test fails the script exits with a non-zero status and shows
# diagnostics on standard output.
#
# You can use this script with git-bisect. See the documentation at
#
#     http://git-scm.com/book/en/Git-Tools-Debugging-with-Git
#
# for an example of using a script like this with the 'git bisect run'
# command.
test: clean all
	$(EASK) install
	$(EASK) test ert wiz-test.el

.PHONY: all autoloads clean test
