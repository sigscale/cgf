# Docker

Get started with SigScale's Charging Gateway Funcion (CGF) running in a Docker container image:
```
$ docker pull sigscale/cgf
$ docker run -ti --entrypoint bash -h host1 -v db:/home/otp/db sigscale/cgf
otp@host1:~$ bin/initialize
otp@host1:~$ exit
$ docker run -ti -h host1 -v db:/home/otp/db -p 8080:8080/tcp -p 8989:8989/tcp sigscale/cgf
```

## Support
Contact <support@sigscale.com> for further assistance.

