#!/bin/bash

usage()
{
cat << EOF
usage: $0 options

This script launches up DubDub.  Hang on.

OPTIONS:
   -h      Show this message
   -b      This is a boot (or master) node
   -n      Put the name of the node here.
   -w      Start as a worker node
   -m      Nodename of master node (for workers)
EOF
}

BOOT=
WORKER=

ERL_OPTS="-kernel inet_dist_listen_min 4000 inet_dist_listen_max 4200 +P1000000 -s dubdub_app -setcookie mysecretcookie"


while getopts “h:m:b:n:dw” OPTION
do
     case $OPTION in
	 h)
             usage
             exit 1
             ;;
	 m)
            MASTER=$OPTARG
            ;;
	 b)
             BOOT=$OPTARG
             ;;
	 n)
             NAME=$OPTARG
             ;;
	 w)
             WORKER=$OPTARG
             ;;
         d)
             DETACHED="-detached"
             ;;
	 ?)
             usage
             exit
             ;;
     esac
done

#if [[ -z $BOOT ]] || [[ -z $WORKER ]] || [[ -z $PASSWD ]] || [[ -n $NAME ]]
#then
#     usage
#     exit 1
#fi

# Feed master nodename to slaves
if test -z "$MASTER"
then
    echo "erl -name $NAME $ERL_OPTS $DETACHED"
    erl -name $NAME $ERL_OPTS $DETACHED
else
    echo "erl -m $MASTER -name $NAME $ERL_OPTS $DETACHED"
    erl -m $MASTER -name $NAME $ERL_OPTS $DETACHED
fi
