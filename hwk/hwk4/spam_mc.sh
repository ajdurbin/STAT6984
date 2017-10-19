#!/bin/bash

var1=$1
echo you entered $var1

if [[ -n ${var1//[0-9]/} ]]; then
    echo "NOT A NUMBER"
    exit
fi

for ((i=0; i<=$var1; i++))
do 
    echo $i
done

