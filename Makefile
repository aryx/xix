###############################################################################
# Prelude
###############################################################################

# Note that we are using 'mk', instead of 'make', for most of the build
# infrastructure of xix, so look more in './mkfile' if you want to adjust
# the build process.
#
# We are using 'mk' because we implemented it in OCaml (see the code under mk/)
# and it's good to dogfood. Moreover, one of the goal of xix is to be
# self-sufficient so it is better to limit external dependencies.
# However, it's still convenient to have Makefile targets for non-critical
# tasks, since 'mk' is not a standard tool, hence this file.
# 
# For the same reason, we are also unfortunately not using 'dune' even though
# it is extremely nice and convenient for OCaml development.

###############################################################################
# Main targets
###############################################################################

all:
	@echo use ./bootstrap-mk.sh and then BOOTSTRAP/mk instead of make

clean:
	git clean -fX

###############################################################################
# Developer targets
###############################################################################

pr:
	git push origin `git rev-parse --abbrev-ref HEAD`
	hub pull-request -b master

push:
	git push origin `git rev-parse --abbrev-ref HEAD`

merge:
	A=`git rev-parse --abbrev-ref HEAD` && git checkout master && git pull && git branch -D $$A

# See https://github.com/aryx/codemap
visual:
	codemap -screen_size 3 -filter semgrep -efuns_client efuns_client -emacs_client /dev/null .
