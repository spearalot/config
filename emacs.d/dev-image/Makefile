uid=$(shell id -u)
gid=$(shell id -g)

all:
	docker build --build-arg USER=$(USER) --build-arg ID=$(uid) --build-arg GID=$(gid) -t dev-image .
