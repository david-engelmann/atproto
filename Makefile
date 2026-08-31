INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)
LOCAL_PDS_ENV := ATP_SCHEME=http ATP_HOST=localhost:2583 ATP_AUTH=alice.test:local-pds-ci-password ATP_LOCAL_PDS=1 ATP_REQUIRE_LOCAL_PDS=1

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

pds-up:
	./scripts/local-pds.sh up

pds-down:
	./scripts/local-pds.sh down

pds-account:
	./scripts/local-pds.sh account

pds-logs:
	./scripts/local-pds.sh logs

# Local laptop without Docker: skip (exit 0). CI calls pds-up + test-pds-run
# directly and must not skip.
test-pds:
	@if ! command -v docker >/dev/null 2>&1 || ! docker info >/dev/null 2>&1; then \
		echo "Docker not available; skipping local PDS integration tests"; \
		exit 0; \
	fi
	$(MAKE) pds-up
	./scripts/local-pds.sh account
	$(MAKE) test-pds-run

test-pds-run:
	$(LOCAL_PDS_ENV) dune exec -- test/test_local_pds.exe

.PHONY: default install uninstall reinstall clean test pds-up pds-down pds-account pds-logs test-pds test-pds-run
