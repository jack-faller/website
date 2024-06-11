.PHONY: all clean send image tiling/clean

all: servdir
send: servdir nginx-token
	rsync --recursive --compress servdir/ "$$(cat server-url.txt):/website/" --delete-after
clean: tiling/clean
	rm -rf servdir nginx-token generated || true
image: tiling/output.png
servdir: generated image $(shell find static) server-url.txt
	rm -rf servdir || true
	cp -rT static servdir
	cp -rT generated/ servdir
	mkdir -p servdir/images
	cp tiling/output.png servdir/images/tiling_pattern.png
generated: $(shell find pages posts thoughts) doclisp.scm make.scm
	guile doclisp.scm
nginx-token: nginx.conf server-url.txt
	rsync nginx.conf "$$(cat server-url.txt):/etc/nginx/nginx.conf"
	ssh $$(cat server-url.txt) nginx -s reload
	echo "up-to-date" > nginx-token

flags := $(shell pkg-config --libs --cflags gl)
flags += $(shell pkg-config --libs --cflags glfw3)
flags += $(shell pkg-config --libs --cflags glm)
glad_dir = tiling/glad
glad_flags = -I$(glad_dir)/include -ldl
$(glad_dir): tiling/glad-flags.txt
	rm -rf $@
	glad $(shell cat $<) --out-path $@
$(glad_dir)/src/gl.c: $(glad_dir)
tiling/main: tiling/main.cpp $(glad_dir)/src/gl.c
	g++ -ggdb -Wall $(glad_flags) $(flags) $^ -o $@
tiling/output.ppm: tiling/main tiling/*.vert tiling/*.frag
	$< --output $@
tiling/output.png: tiling/output.ppm
	magick $< $@
tiling/clean:
	rm -rf $(glad_dir) tiling/main tiling/output.ppm tiling/output.png
