workdir = day29
host = beta.tunbury.uk

build:
	dune fmt
	dune build --release

install:
	dune fmt
	dune build --release
	ssh $(host) "rm -f obi.exe"
	scp _build/default/bin/main.exe $(host):obi.exe
	ssh $(host) "rm -f opamh.exe"
	scp ../opamh/_build/default/opamh.exe $(host):
	ssh $(host) "rm -f opam-build"
	scp ../opam_build/_build/default/bin/main.exe $(host):opam-build
	scp Makefile $(host):
	scp setup.json $(host):

setup:	opam-repository
	mkdir -p $(workdir)
	sudo chown $$(id -u):$$(id -g) $(workdir)
	mkdir -p download-cache
	sudo chown 1000:1000 download-cache
	mkdir -p $(workdir)/dummy
	mkdir -p $(workdir)/work
	mkdir -p $(workdir)/temp
	sudo mkdir -p $(workdir)/rootfs
	docker export $$(docker run -d debian:12) | sudo tar -C $(workdir)/rootfs -x
	sudo cp opamh.exe $(workdir)/rootfs/usr/local/bin
	sudo cp opam-build $(workdir)/rootfs/usr/local/bin
	sudo curl -L https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-x86_64-linux -o $(workdir)/rootfs/usr/local/bin/opam
	sudo chmod +x $(workdir)/rootfs/usr/local/bin/opam
	sudo chown -R 1000:1000 opam-repository
	git config --global --add safe.directory /root/opam-repository
	git -C opam-repository pull origin master
	cp setup.json $(workdir)/config.json
	sudo mkdir $(workdir)/rootfs/etc/sudoers.d
	echo "opam ALL=(ALL:ALL) NOPASSWD:ALL" | sudo tee $(workdir)/rootfs/etc/sudoers.d/opam
	echo "127.0.0.1 localhost builder" > $(workdir)/hosts
	sudo runc run --bundle $(workdir) setup

opam-repository:
	git clone https://github.com/ocaml/opam-repository

clean:
	sudo rm -rf $(workdir)

%.pdf: %.dot
	dot -Tpdf -o $@ $<

serve:
	python3 -m http.server --directory $(workdir)/html/ --bind 0.0.0.0 8080

# New targets for the restructured CLI
obi-setup:
	dune exec -- obi setup --work-dir $(workdir)

obi-solve:
	dune exec -- obi solve --work-dir $(workdir)

obi-build:
	dune exec -- obi build --work-dir $(workdir)

obi-report:
	dune exec -- obi report --work-dir $(workdir)

obi-run:
	dune exec -- obi run --work-dir $(workdir)

obi-serve:
	dune exec -- obi serve --work-dir $(workdir)
