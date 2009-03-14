#!/bin/bash

COUNTER=1
while [ $COUNTER -lt $2 ]; do
	echo Counter is $COUNTER
	cat $1 >> data.txt
	let COUNTER=COUNTER+1
done
