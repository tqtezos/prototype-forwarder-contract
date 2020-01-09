.PHONY: build-linux64-static build-linux64-static-tar build-docker-image \
	build-docker-image-tar build-local build-local-tar build-all \
	release-candidate print-vars hashsum-files

NAME := $(shell cat package.yaml | grep "name:" | sed 's/name: *//g')
VERSION := $(shell cat package.yaml | grep "version:" | sed 's/version: *//g')
DIST := $(PWD)/dist
OS_NAME := $(shell uname -s | tr '[:upper:]' '[:lower:]')
OS_ARCH := $(shell uname -m)

BUILD_NUMBER := $(shell scripts/nextbuildnumber.sh ${VERSION})
FULL_VERSION := ${VERSION}-${BUILD_NUMBER}
DOCKER_TAG := ${NAME}:${FULL_VERSION}

OUTPUT_DIR := ${NAME}-${FULL_VERSION}-${OS_NAME}-${OS_ARCH}
FULL_OUTPUT_DIR := ${DIST}/${OUTPUT_DIR}

LINUX64_OUTPUT_DIR := ${NAME}-${FULL_VERSION}-linux-x86_64-static
LINUX64_FULL_OUTPUT_DIR := ${DIST}/${LINUX64_OUTPUT_DIR}

LINUX64_STATIC_TAR := ${DIST}/${LINUX64_OUTPUT_DIR}.tar.gz
LOCAL_TAR  := ${DIST}/${OUTPUT_DIR}.tar.gz
DOCKER_IMAGE_TAR := ${DIST}/${NAME}-${FULL_VERSION}-docker.tar.gz

build-linux64-static:
	mkdir -p ${LINUX64_FULL_OUTPUT_DIR}
	docker run --rm \
		-v $(PWD):/usr/src/build \
		-v $(HOME)/.stack:/root/.stack \
		-v $(HOME)/.ssh:/root/.ssh \
		-v ${LINUX64_FULL_OUTPUT_DIR}:/root/.local/bin \
		-w /usr/src/build -it itkach/alpine-haskell-stack19-upx:8.6.5-2 \
		bash -c "stack config set system-ghc --global true && \
			 stack install --ghc-options='-optl-static -fPIC -optc-Os' && \
			 upx /root/.local/bin/*"

build-linux64-static-tar: build-linux64-static
	tar -C ${DIST} -czvf ${LINUX64_STATIC_TAR} ${LINUX64_OUTPUT_DIR}

build-docker-image: build-linux64-static
	docker build . --tag ${NAME} --tag ${DOCKER_TAG}

build-docker-image-tar: build-docker-image
	docker save ${DOCKER_TAG} | gzip --best -c > ${DOCKER_IMAGE_TAR}

build-local:
	mkdir -p ${FULL_OUTPUT_DIR}
	stack install --local-bin-path ${FULL_OUTPUT_DIR}

build-local-tar: build-local
	tar -C ${DIST} -czvf ${LOCAL_TAR} ${OUTPUT_DIR}

build-all: build-linux64-static-tar build-docker-image-tar build-local-tar

hashsum-files:
	scripts/sha256.sh ${DIST}/

release-candidate: build-all hashsum-files
	hub release create -m ${FULL_VERSION} --prerelease \
	-a ${LINUX64_STATIC_TAR} \
	-a ${LINUX64_STATIC_TAR}.sha256 \
	-a ${LOCAL_TAR} \
	-a ${LOCAL_TAR}.sha256 \
	-a ${DOCKER_IMAGE_TAR} \
	-a ${DOCKER_IMAGE_TAR}.sha256 \
	v${FULL_VERSION}

print-vars:
	@echo "NAME: ${NAME}"
	@echo "VERSION: ${VERSION}"
	@echo "BUILD_NUMBER: ${BUILD_NUMBER}"
	@echo "DIST: ${DIST}"
	@echo "OS_NAME: ${OS_NAME}"
	@echo "OS_ARCH: ${OS_ARCH}"
	@echo "DOCKER_TAG: ${DOCKER_TAG}"
	@echo "DOCKER_IMAGE_TAR: ${DOCKER_IMAGE_TAR}"
