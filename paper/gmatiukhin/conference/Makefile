COMMON_PARAMS=-v $(PWD):/home/marp/app/ -e MARP_USER="$(shell id -u):$(shell id -g)" -e LANG=$(LANG)

all: html pdf

html:
	docker run --rm $(COMMON_PARAMS) marpteam/marp-cli --html --allow-local-files pres.md

pdf:
	docker run --rm --init $(COMMON_PARAMS) marpteam/marp-cli pres.md --html --pdf --allow-local-files

watch:
	docker run --rm --init $(COMMON_PARAMS) -p 37717:37717 marpteam/marp-cli -w --html pres.md

server:
	docker run --rm --init $(COMMON_PARAMS) -p 8080:8080 -p 37717:37717 marpteam/marp-cli -s . --html
