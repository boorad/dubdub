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
EOF
}

BOOT=
WORKER=
while getopts “h:b:n:w” OPTION
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
echo "erl -sname $NAME -s dubdub_app"
erl -sname $NAME -s dubdub_app
