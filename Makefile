.PHONY: all clean
all: servdir nginx-token
servdir: generated data $(shell find data) server-url.txt
	rsync --recursive --compress data/ generated/ "$$(cat server-url.txt):/data/" --delete-after
	rm -rf servdir || true
	cp -rT data servdir
	cp -rT generated/ servdir
generated: $(shell find pages posts thoughts) doclisp.scm make.scm
	guile doclisp.scm
nginx-token: nginx.conf server-url.txt
	rsync nginx.conf "$$(cat server-url.txt):/etc/nginx/nginx.conf"
	ssh $$(cat server-url.txt) nginx -s reload
	echo "up-to-date" > nginx-token
clean:
	rm -rf servdir nginx-token generated || true
