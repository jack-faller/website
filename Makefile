.PHONY: all clean send image serve log build/log.html

export CC=gcc

all: build/local/output
log: build/log.html
send: build/remote/output build/nginx-token
	rsync --recursive --compress "$</" "$$(cat server-url.txt):/website/" --delete-after
clean:
	mv build/nginx-token .
	rm -rf build || true
	mkdir build
	mv nginx-token build/nginx-token
serve: build/local/output
	guix shell python -- python3 -m http.server -d build/local/output
build/log.html:
	./getlog.sh build/logs-temp
	@echo "Get the latest geo-IP database from http://db-ip.com/db/download/ip-to-city-lite and put it in ./build/geo-ip.mmdb"
	with_args () {\
		goaccess build/logs-temp/* -o "$@" --log-format=COMBINED --ignore-crawlers $$@;\
	};\
	if [ -f ./build/geo-ip.mmdb ]; then\
		with_args --geoip-database=./build/geo-ip.mmdb;\
	else\
		echo "Geo-IP database file was missing" >&2;\
		with_args;\
	fi
	rm -rf build/logs-temp
	xdg-open "$@"
build/generic: build/generic/images/tiling_pattern.png
build/%/output: build/%/generated build/generic $(shell find static) server-url.txt
	rm -rf "$@" || true
	cp -rT static "$@"
	cp -rT build/generic "$@"
	cp -rT "build/$*/generated" "$@"
build/%/generated: blogroll.opml $(shell find pages posts) doclisp.scm make.scm maths.scm highlight.sh tree-sitter-config.json
	rm -rf "$@" || true
	sed "s#{{PROJECT_ROOT}}#$$PWD#" tree-sitter-config.json > build/tree-sitter-config.json
	BUILD_TYPE="$*" ./guile.sh -e "(@ (make) build)" make.scm . "$@"
	mv "build/$*/generated/modify-blogroll.xsl" "build/$*" || true
	sed 's#<opml#<?xml version="1.0" encoding="UTF-8"?><opml xmlns="http://opml.org/spec2"#' "$<" > "build/$*/blogroll.opml"
	xsltproc -o "$@/blogroll.xml" "build/$*/modify-blogroll.xsl" "build/$*/blogroll.opml"
build/%/generated/blogroll.xml: blogroll.opml build/%/generated
	mkdir -p "$$(dirname "$@")"
build/nginx-token: nginx.conf server-url.txt
	mkdir -p "$$(dirname "$@")"
	rsync nginx.conf "$$(cat server-url.txt):/etc/nginx/nginx.conf"
	ssh $$(cat server-url.txt) nginx -s reload
	echo "up-to-date" > nginx-token

flags := $(shell pkg-config --libs --cflags gl)
flags += $(shell pkg-config --libs --cflags glfw3)
flags += $(shell pkg-config --libs --cflags glm)
glad_dir = build/glad
glad_flags = -I$(glad_dir)/include -ldl
$(glad_dir): tiling/glad-flags.txt
	rm -rf "$@"
	glad $(shell cat "$<") --out-path "$@"
$(glad_dir)/src/gl.c: $(glad_dir)
build/main: tiling/main.cpp $(glad_dir)/src/gl.c
	mkdir -p "$$(dirname "$@")"
	g++ -ggdb -Wall $(glad_flags) $(flags) $^ -o "$@"
build/output.ppm: build/main tiling/*.vert tiling/*.frag
	"$<" --output "$@"
build/generic/images/tiling_pattern.png: build/output.ppm
	mkdir -p "$$(dirname "$@")"
	convert "$<" "$@"
