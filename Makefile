EMACS ?= emacs
EASK ?= eask

.PHONY: clean package install compile test checkdoc lint

# CI entry point
ci: clean package install compile checkdoc lint

# Build an package artefact, default to `dist` folder
#
# This is used to test if your package can be built correctly before the
# package installation.
package:
	@echo "Packaging..."
	$(EASK) package

# Install package
#
# If your package is a single file package, you generally wouldn't need to
install:
	@echo "Installing..."
	$(EASK) install

# Byte-compile package
#
# Compile all your package .el files to .elc
compile:
	@echo "Compiling..."
	$(EASK) compile

# Run regression tests
#
# The default test is `ert`; but Eask also support other regression test!
# See https://emacs-eask.github.io/Getting-Started/Commands-and-options/#-linter
test:
	@echo "Testing..."
	$(EASK) install-deps --dev
	$(EASK) test ert ./test/*.el

# Run checkdoc
#
# See https://www.emacswiki.org/emacs/CheckDoc
checkdoc:
	@echo "Checking documentation..."
	$(EASK) lint checkdoc --strict

# Lint package metadata
#
# See https://github.com/purcell/package-lint
lint:
	@echo "Linting..."
	$(EASK) lint package

# Clean up
#
# This will clean all the entire workspace including the following folders
# and files
#
#   - .eask folder (sandbox)
#   - all .elc files
clean:
	$(EASK) clean-all
