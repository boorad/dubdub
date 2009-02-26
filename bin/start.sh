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
while getopts “h:b:n:w:m” OPTION
do
     case $OPTION in
         h)
             usage
             exit 1
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
             
         m)
            MASTER=$OPTARG
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
if [ -z "$MASTER" ]
then
    echo "erl -sname $NAME -s dubdub_app -cookie cookie"
    erl -sname $NAME -s dubdub_app
else
    echo "erl -sname $NAME -s dubdub_app -cookie cookie -m $MASTER"
    erl -sname $NAME -s dubdub_app -m $MASTER
fi
