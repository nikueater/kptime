

all: server

build:
	npm install; npx parcel build src/index.html --out-dir dist

server:
	npx parcel src/index.html --out-dir dist
