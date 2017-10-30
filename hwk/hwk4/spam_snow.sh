#!/bin/bash

# check if 0, 1, or more than 1 argument
if [[ $# -eq 0 ]];
then 
    value=4
elif [[ $# -gt 1 ]];
then
    echo "TOO MANY ARGUMENTS, USING 4 CORES"
    value=4
else
    value=$1
fi

R CMD BATCH "--args cores=$value" spam_snow.R
