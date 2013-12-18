PROJECT = prp

DEPS = cowboy jsx
dep_cowboy = pkg://cowboy master
dep_jsx = pkg://jsx master

include erlang.mk

########
# extras
########
api-tests-quick:
	$(shell ./_rel/bin/prp start 1>&2; sleep 1; python -m unittest discover test '*_tests.py' -v; result=$$?; ./_rel/bin/prp stop 1>&2; exit $$result )

api-tests:
	$(shell python -m unittest discover test '*_tests.py' -v )

# for interactive use:
# run-console:
#	./_rel/bin/prp console

# unit-tests:
#	@./rebar skip_deps=true eunit
