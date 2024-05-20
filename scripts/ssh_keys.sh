#!/bin/bash
# Create SSH keys

ALGORITHMS="rsa dsa ecdsa ed25519"

system_keys() {
	if ! [ -d ${system_dir} ] ;
	then
		mkdir -p ${system_dir}
	fi
	for algo in ${ALGORITHMS};
	do
		if ! [ -f ${system_dir}/ssh_host_${algo}_key ] ;
		then
			ssh-keygen -t ${algo} -N '' -f ${system_dir}/ssh_host_${algo}_key
		fi
	done
}

user_keys() {
	for username in $@;
	do
		if ! [ -d ${user_dir}/${username} ] ;
		then
			mkdir -p ${user_dir}/${username}
		fi
		for algo in ${ALGORITHMS};
		do
			if ! [ -f ${user_dir}/${username}/id_${algo} ] ;
			then
				ssh-keygen -t ${algo} -N '' -f ${user_dir}/${username}/id_${algo}
			fi
			cat ${user_dir}/${username}/id_${algo}.pub >> ${user_dir}/${username}/authorized_keys
		done
	done
}

usage() {
	echo "usage: $0 [--system-dir ~/ssh/system]"
	echo "usage: $0 [--user-dir ~/ssh/system/user] USERNAME ..."
	echo "usage: $0 --help"
}

system_dir="${HOME}/ssh/system"
user_dir="${HOME}/ssh/user"

while [ -n "$1" ];
do
	case $1 in
		--system-dir )
			shift
			system_dir="$1"
			;;
		--user-dir )
			shift
			user_dir="$1"
			;;
		--help )
			usage
			exit
			;;
		* )
			break
			;;
	esac
	shift
done

if [ $# -gt 0 ] ;
then
	user_keys $@
else
	system_keys
fi

