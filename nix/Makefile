.DEFAULT_GOAL = help

require-%:
	if [ "${${*}}" = "" ]; then \
	        @echo "ERROR: Environment variable not set: \"$*\""; \
	        @exit 1; \
	fi

## Install packages
install:
	nix-env -i -f ./packages.nix

## Uninstall package
uninstall: require-PKG
	nix-env -e ${PKG}

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n"
	@awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
