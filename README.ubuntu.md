# Don't knock yourself out! Production ready debian packages are available.

## Install SigScale package repository configuration:

### Ubuntu 24.04 LTS (noble)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-noble/sigscale-release_1.4.5-1+ubuntu24.04_all_dccb359ae044de02cad8f728fb007658.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

### Ubuntu 22.04 LTS (jammy)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-jammy/sigscale-release_1.4.5-1+ubuntu22.04_all_46d1cecfa87e978a64becc2cb0081fc3.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

## Install SigScale CGF:
	sudo apt install sigscale-cgf
	sudo systemctl enable cgf
	sudo systemctl start cgf
	sudo systemctl status cgf

Contact <support@sigscale.com> for further assistance.

