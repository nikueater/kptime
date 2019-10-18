

all: server


server:
	npx parcel src/index.html --out-dir dist
