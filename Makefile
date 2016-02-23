PROJECT = steam
PROJECT_DESCRIPTION = Search Tags in Erlang Application or Module
PROJECT_VERSION = 0.0.1
DEPS = geas

include erlang.mk

clean::
	@-find . -name "*~" -delete
