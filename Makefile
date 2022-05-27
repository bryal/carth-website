.PHONY: all
all: upload

.PHONY: upload
upload:
	rsync -avP --chown "jojo:httpd" --chmod=Du=rwx,Dg=rx,Do=rx,Fu=rw,Fg=r,Fo=r --exclude=".*" --exclude="Makefile" ./ odin:/srv/http/carth/
