#!/usr/bin/env python3
from http.server import SimpleHTTPRequestHandler
from socketserver import TCPServer
from sys import argv

def Handler(request, client_address, server, directory=None):
    out = SimpleHTTPRequestHandler
    out.extensions_map = {'.xhtml': 'application/xhtml+xml'}
    directory=argv[1] if len(argv) == 2 else directory
    return out(request, client_address, server, directory=directory)

def run():
    with TCPServer(('', 8000), Handler) as httpd:
        httpd.serve_forever()


if __name__ == '__main__':
    run()
