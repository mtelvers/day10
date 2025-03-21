workdir = day29

build:
	dune fmt
	dune build --release

install:
	dune fmt
	dune build --release
	ssh mtelvers@x86-bm-c9.sw.ocaml.org "rm -f main.exe"
	scp _build/default/bin/main.exe mtelvers@x86-bm-c9.sw.ocaml.org:
	scp Makefile mtelvers@x86-bm-c9.sw.ocaml.org:
	scp setup.json mtelvers@x86-bm-c9.sw.ocaml.org:

setup:	opam-repository
	fallocate -l 2048GiB $(workdir).xfs
	mkfs.xfs -i maxpct=100 $(workdir).xfs
	mkdir $(workdir)
	sudo mount $(workdir).xfs $(workdir)
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
	sudo chown -R mtelvers:mtelvers opam-repository
	git -C opam-repository pull origin master
	cp setup.json $(workdir)/config.json
	sudo mkdir $(workdir)/rootfs/etc/sudoers.d
	echo "opam ALL=(ALL:ALL) NOPASSWD:ALL" | sudo tee $(workdir)/rootfs/etc/sudoers.d/opam
	echo "127.0.0.1 localhost builder" > $(workdir)/hosts
	sudo runc run --bundle $(workdir) setup

opam-repository:
	git clone https://github.com/ocaml/opam-repository

clean:
	sudo umount $(workdir)
	rmdir $(workdir)
	rm $(workdir).xfs

%.pdf: %.dot
	dot -Tpdf -o $@ $<
