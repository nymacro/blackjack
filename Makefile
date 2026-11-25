SHELL := /bin/sh

STACK           ?= stack
IMAGE           ?= mmaker
TAG             ?= latest
PORT            ?= 3000
CONTAINER_ENGINE ?= $(shell command -v podman 2>/dev/null || command -v docker 2>/dev/null)

.PHONY: all help build test run clean distclean image container container-run container-shell

all: build

help:
	@printf '%s\n' \
		'Usage: make <target>' \
		'' \
		'  build           Build the project with Stack' \
		'  test            Run the test suite' \
		'  run             Start the server on port 3000' \
		'  clean           Remove this package'"'"'s build output' \
		'  distclean       Remove all local Stack build output' \
		'  image           Build the container image ($(IMAGE):$(TAG))' \
		'  container       Alias for image' \
		'  container-run   Run the container on port $(PORT)' \
		'  container-shell Open a shell in the container image' \
		'  help            Show this help' \
		'' \
		'Variables: STACK=$(STACK) IMAGE=$(IMAGE) TAG=$(TAG) PORT=$(PORT)' \
		'           CONTAINER_ENGINE=$(CONTAINER_ENGINE)'

build:
	$(STACK) build

test:
	$(STACK) test

run:
	$(STACK) exec mmaker

clean:
	$(STACK) clean

distclean:
	$(STACK) clean --full

image:
	@if [ -z "$(CONTAINER_ENGINE)" ]; then \
		echo "error: neither podman nor docker was found in PATH" >&2; \
		exit 1; \
	fi
	$(CONTAINER_ENGINE) build -f Containerfile -t $(IMAGE):$(TAG) .

container: image

container-run:
	@if [ -z "$(CONTAINER_ENGINE)" ]; then \
		echo "error: neither podman nor docker was found in PATH" >&2; \
		exit 1; \
	fi
	$(CONTAINER_ENGINE) run --rm -p $(PORT):3000 $(IMAGE):$(TAG)

container-shell:
	@if [ -z "$(CONTAINER_ENGINE)" ]; then \
		echo "error: neither podman nor docker was found in PATH" >&2; \
		exit 1; \
	fi
	$(CONTAINER_ENGINE) run --rm -it --entrypoint /bin/sh $(IMAGE):$(TAG)
