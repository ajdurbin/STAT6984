#!/bin/bash

# check if 0 or more than 1 arguments
if [[ $# -eq 0 ]];
then
    value=2
elif [[ $# -gt 1 ]];
then
    echo "TOO MANY ARGUMENTS"
    exit
fi

# if single argument, validate and update
if [[ $# -eq 1 ]];
then
    value=$1
    if [[ -n ${value//[0-9]/} ]];
    then
        echo "INVALID ARGUMENT"
        exit
    fi
fi

# now call r scripts
for((i=1; i<=$value; i++))
do
    nohup R CMD BATCH "--args seed=$i reps=5" spam_mc.R spam_mc_$i.Rout &
done
