.DEFAULT_GOAL = help

# https://stackoverflow.com/a/14061796
# If the first argument is "search"...
ifeq (search,$(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "search"
  SEARCH_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(SEARCH_ARGS):;@:)
endif

require-%:
	if [ "${${*}}" = "" ]; then \
	        @echo "ERROR: Environment variable not set: \"$*\""; \
	        @exit 1; \
	fi

## Install packages
install:
	nix-env -i -f ./packages.nix

## List installed packages
list-packages:
	nix-env -q

## Install specific package
install-pkg: require-PKG
	nix-env -i --attr ${PKG} -f ./packages.nix

## Uninstall package
uninstall: require-PKG
	nix-env -e ${PKG}

## Search for a package
search:
	nix search $(SEARCH_ARGS)

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
