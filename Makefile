###############################################################################
# Prelude
###############################################################################

# Note that we are using 'mk', instead of 'make', for the build infrastructure
# of xix, so look more in './mkfile' if you want to adjust the build process.
#
# We are using 'mk' because we implemented it in OCaml (see the code under mk/)
# and it's good to dogfood. Moreover, one of the goal of xix is to be
# self-sufficient so it is better to limit external dependencies.
#
# However, it's still convenient to have Makefile targets for non-critical
# tasks, since 'mk' is not a standard tool, hence this file.
# Moreover, you can build xix also with dune so we can leverage merlin
# indexing in editors. However this is optional! It is important to be able to
# build xix without dune and with just mk!

###############################################################################
# Main targets
###############################################################################

all:
	dune build
	@echo you can also use ./bootstrap-mk.sh and ./bin/mk instead of make

#alt: git clean -fX
clean:
	dune clean
	mk clean

bootstrap:
	./bootstrap-mk.sh
promote:
	cp mk/mk shell/rc bin/

build-docker:
	docker build -t "xix" .
.PHONY: shell
shell:
	nix-shell --pure

.PHONY: test
test:
	dune build
	./test

###############################################################################
# Developer targets
###############################################################################

# see https://github.com/semgrep/semgrep
check:
	osemgrep --experimental --config semgrep.jsonnet --strict --error

pr:
	git push origin `git rev-parse --abbrev-ref HEAD`
	hub pull-request -b master
push:
	git push origin `git rev-parse --abbrev-ref HEAD`
merge:
	A=`git rev-parse --abbrev-ref HEAD` && git checkout master && git pull && git branch -D $$A

# See https://github.com/aryx/codemap and https://github.com/aryx/fork-efuns
visual:
	codemap -screen_size 3 -filter semgrep -efuns_client efuns_client -emacs_client /dev/null .
