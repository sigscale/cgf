#!/bin/bash
# Install TLS certificates

if [ ! -d $HOME/tls ];
then
	mkdir $HOME/tls
fi
if [ ! -f $HOME/tls/cert.pem ];
then
	echo "Creating TLS certificates."
	openssl req -newkey rsa:2048 -nodes -x509 -days 1024 \
		-subj /C=LK/L=Colombo/O=SigScale/CN=ca.$(hostname)\/emailAddress=support@$(hostname) \
		-keyout $HOME/tls/cakey.pem \
		-out $HOME/tls/ca.pem
	openssl req -newkey rsa:2048 -nodes \
		-subj /C=LK/L=Colombo/O=SigScale/CN=$(hostname)\/emailAddress=support@$(hostname) \
		-keyout $HOME/tls/key.pem \
		-out $HOME/tls/cert.csr
	echo "extendedKeyUsage = serverAuth" > $HOME/tls/extensions
	echo "subjectAltName = DNS:$(hostname)" >> $HOME/tls/extensions
	openssl x509 -req -days 1024 \
		-CA $HOME/tls/ca.pem \
		-CAkey $HOME/tls/cakey.pem \
		-CAcreateserial \
		-extfile $HOME/tls/extensions \
		-in $HOME/tls/cert.csr \
		-out $HOME/tls/cert.pem
	openssl x509 -outform DER \
		-in $HOME/tls/ca.pem \
		-out $HOME/tls/ca.der
	chmod 400 $HOME/tls/key.pem $HOME/tls/cakey.pem
else
	echo "Leaving existing TLS certificates in place."
fi

