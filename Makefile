.PHONY: all clean send image

all: servdir
send: servdir nginx-token
	rsync --recursive --compress servdir/ "$$(cat server-url.txt):/website/" --delete-after
clean:
	rm -rf servdir build nginx-token generated || true
image: generated/images/tiling_pattern.png
servdir: generated generated/blogroll.xml image $(shell find static) server-url.txt
	rm -rf servdir || true
	cp -rT static servdir
	cp -rT generated/ servdir
generated: $(shell find pages posts) doclisp.scm make.scm maths.scm
	rm -rf "$@"
	./guile.sh -e "(@ (make) build)" make.scm . "$@"
generated/blogroll.xml: blogroll.opml generated
	mkdir -p "$$(dirname "$@")"
	mv generated/modify-blogroll.xsl build/ || true
	sed 's#<opml#<?xml version="1.0" encoding="UTF-8"?><opml xmlns="http://opml.org/spec2"#' "$<" > build/blogroll.opml
	xsltproc -o "$@" build/modify-blogroll.xsl build/blogroll.opml
nginx-token: nginx.conf server-url.txt
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
generated/images/tiling_pattern.png: build/output.ppm
	mkdir -p "$$(dirname "$@")"
	convert "$<" "$@"
