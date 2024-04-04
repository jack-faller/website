.PHONY: all clean send image tiling/clean

all: servdir nginx-token
send: servdir
	rsync --recursive --compress servdir/ "$$(cat server-url.txt):/website/" --delete-after
clean: tiling/clean
	rm -rf servdir nginx-token generated || true
image: tiling/output.png
servdir: generated image $(shell find static) server-url.txt
	rm -rf servdir || true
	cp -rT static servdir
	cp -rT generated/ servdir
	cp tiling/output.png servdir/images/tiling_pattern.png
generated: $(shell find pages posts thoughts) doclisp.scm make.scm
	guile doclisp.scm
nginx-token: nginx.conf server-url.txt
	rsync nginx.conf "$$(cat server-url.txt):/etc/nginx/nginx.conf"
	ssh $$(cat server-url.txt) nginx -s reload
	echo "up-to-date" > nginx-token

flags := $(shell pkg-config --libs --cflags gl glfw3)
glad_dir = tiling/glad
glad_flags = -I$(glad_dir)/include -ldl
$(glad_dir): tiling/glad-flags.txt
	rm -rf $@
	glad $(shell cat $<) --out-path $@
$(glad_dir)/src/gl.c: $(glad_dir)
tiling/main: tiling/main.cpp $(glad_dir)/src/gl.c
	g++ -Wall -lc $(flags) $(glad_flags) $^ -o $@
tiling/output.ppm: tiling/main tiling/*.vert tiling/*.frag
	$< --output $@
tiling/output.png: tiling/output.ppm
	convert $< $@
tiling/clean:
	rm -rf $(glad_dir) tiling/main tiling/output.ppm tiling/output.png
