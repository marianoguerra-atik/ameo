BASEDIR = $(shell pwd)
REBAR = rebar3
RELPATH = _build/default/rel/ameo
PRODRELPATH = _build/prod/rel/ameo
DEV1RELPATH = _build/dev1/rel/ameo
DEV2RELPATH = _build/dev2/rel/ameo
DEV3RELPATH = _build/dev3/rel/ameo
APPNAME = ameo
SHELL = /bin/bash

release:
	$(REBAR) release
	mkdir -p $(RELPATH)/../ameo_config
	[ -f $(RELPATH)/../ameo_config/ameo.conf ] || cp $(RELPATH)/etc/ameo.conf  $(RELPATH)/../ameo_config/ameo.conf
	[ -f $(RELPATH)/../ameo_config/advanced.config ] || cp $(RELPATH)/etc/advanced.config  $(RELPATH)/../ameo_config/advanced.config

console:
	cd $(RELPATH) && ./bin/ameo console

prod-release:
	$(REBAR) as prod release
	mkdir -p $(PRODRELPATH)/../ameo_config
	[ -f $(PRODRELPATH)/../ameo_config/ameo.conf ] || cp $(PRODRELPATH)/etc/ameo.conf  $(PRODRELPATH)/../ameo_config/ameo.conf
	[ -f $(PRODRELPATH)/../ameo_config/advanced.config ] || cp $(PRODRELPATH)/etc/advanced.config  $(PRODRELPATH)/../ameo_config/advanced.config

prod-console:
	cd $(PRODRELPATH) && ./bin/ameo console

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) ct

devrel1:
	$(REBAR) as dev1 release
	mkdir -p $(DEV1RELPATH)/../ameo_config
	[ -f $(DEV1RELPATH)/../ameo_config/ameo.conf ] || cp $(DEV1RELPATH)/etc/ameo.conf  $(DEV1RELPATH)/../ameo_config/ameo.conf
	[ -f $(DEV1RELPATH)/../ameo_config/advanced.config ] || cp $(DEV1RELPATH)/etc/advanced.config  $(DEV1RELPATH)/../ameo_config/advanced.config

devrel2:
	$(REBAR) as dev2 release
	mkdir -p $(DEV2RELPATH)/../ameo_config
	[ -f $(DEV2RELPATH)/../ameo_config/ameo.conf ] || cp $(DEV2RELPATH)/etc/ameo.conf  $(DEV2RELPATH)/../ameo_config/ameo.conf
	[ -f $(DEV2RELPATH)/../ameo_config/advanced.config ] || cp $(DEV2RELPATH)/etc/advanced.config  $(DEV2RELPATH)/../ameo_config/advanced.config

devrel3:
	$(REBAR) as dev3 release
	mkdir -p $(DEV3RELPATH)/../ameo_config
	[ -f $(DEV3RELPATH)/../ameo_config/ameo.conf ] || cp $(DEV3RELPATH)/etc/ameo.conf  $(DEV3RELPATH)/../ameo_config/ameo.conf
	[ -f $(DEV3RELPATH)/../ameo_config/advanced.config ] || cp $(DEV3RELPATH)/etc/advanced.config  $(DEV3RELPATH)/../ameo_config/advanced.config

devrel: devrel1 devrel2 devrel3

dev1-console:
	$(BASEDIR)/_build/dev1/rel/ameo/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/ameo/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/ameo/bin/$(APPNAME) console

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/ameo/bin/$(APPNAME) start; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/ameo/bin/$(APPNAME)-admin cluster join ameo1@127.0.0.1; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/ameo/bin/$(APPNAME)-admin cluster plan

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/ameo/bin/$(APPNAME)-admin cluster commit

devrel-status:
	$(BASEDIR)/_build/dev1/rel/ameo/bin/$(APPNAME)-admin member-status

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/ameo/bin/$(APPNAME) ping; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/ameo/bin/$(APPNAME) stop; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach

