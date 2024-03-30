.PHONY: all clean send
servdir: generated $(shell find static) server-url.txt
	rm -rf servdir || true
	cp -rT static servdir
	cp -rT generated/ servdir
all: servdir nginx-token send
send:
	rsync --recursive --compress static/ generated/ "$$(cat server-url.txt):/website/" --delete-after
generated: $(shell find pages posts thoughts) doclisp.scm make.scm
	guile doclisp.scm
nginx-token: nginx.conf server-url.txt
	rsync nginx.conf "$$(cat server-url.txt):/etc/nginx/nginx.conf"
	ssh $$(cat server-url.txt) nginx -s reload
	echo "up-to-date" > nginx-token
clean:
	rm -rf servdir nginx-token generated || true
