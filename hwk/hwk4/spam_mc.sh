#!/bin/bash

var1=$1

if [[ $# -ne 1 ]]; then
    echo "TOO MANY ARGUMENTS"
    exit
fi

if [[ -n ${var1//[0-9]/} ]]; then
    echo "INVALID ARGUMENT"
    exit
fi

for ((i=0; i<=$var1; i++))
do 
    echo $i
done

