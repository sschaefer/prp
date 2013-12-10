PROJECT = prp

DEPS = cowboy
dep_cowboy = pkg://cowboy master

include erlang.mk

########
# extras
########
api-tests:
	$(shell python -m unittest discover test '*_tests.py' -v)

# unit-tests:
#	@./rebar skip_deps=true eunit
