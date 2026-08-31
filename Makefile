INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)
LOCAL_ATP_ENV := ATP_SCHEME=http ATP_HOST=localhost:2583 ATP_APPVIEW_HOST=localhost:2584 ATP_OZONE_HOST=localhost:2587 ATP_AUTH=alice.test:hunter2 ATP_AUTH_BOB=bob.test:hunter2 ATP_AUTH_OZONE=admin-mod.test:admin-mod-pass ATP_LOCAL_PDS=1 ATP_REQUIRE_LOCAL_PDS=1

default:
	dune build

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	dune clean

test:
	dune runtest

pds-up atproto-up:
	./scripts/local-atproto.sh up

pds-down atproto-down:
	./scripts/local-atproto.sh down

pds-account atproto-account:
	./scripts/local-atproto.sh account

pds-logs atproto-logs:
	./scripts/local-atproto.sh logs

# Local laptop without Docker/Node: skip (exit 0). CI calls pds-up + test-pds-run
# directly and must not skip.
test-pds test-atproto:
	@if ! command -v docker >/dev/null 2>&1 || ! docker info >/dev/null 2>&1; then \
		echo "Docker not available; skipping local AT Protocol integration tests"; \
		exit 0; \
	fi
	@if ! command -v node >/dev/null 2>&1 || [ "$$(node -p "process.versions.node.split('.')[0]")" -lt 22 ]; then \
		echo "Node.js >= 22 not available; skipping local AT Protocol integration tests"; \
		exit 0; \
	fi
	$(MAKE) pds-up
	./scripts/local-atproto.sh account
	$(MAKE) test-pds-run

test-pds-run test-atproto-run:
	@set -e; \
	export $(LOCAL_ATP_ENV); \
	if [ -f docker/dev-env/generated.env ]; then set -a; . docker/dev-env/generated.env; set +a; fi; \
	export ATP_REQUIRE_LOCAL_PDS=1 OUNIT_RUNNER=sequential; \
	dune exec -- test/test_local_pds.exe; \
	dune exec -- test/test_local_appview.exe; \
	dune exec -- test/test_local_ozone.exe

.PHONY: default install uninstall reinstall clean test pds-up pds-down pds-account pds-logs atproto-up atproto-down atproto-account atproto-logs test-pds test-atproto test-pds-run test-atproto-run
