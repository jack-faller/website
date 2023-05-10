rsync --recursive --compress data/ generated/ "$1:/data/" --delete-after \
	&& rsync nginx.conf "$1:/etc/nginx/nginx.conf" \
	&& ssh "$1" nginx -s reload
