#!/bin/bash

# Author: Samuele Giraudo
# Creation: apr. 2023
# Modifications: apr. 2023, jun. 2023, jul. 2023

# Usage:
#    ./InterpretAll DIRECTORY "OPTIONS'
# This runs ./aclove --file FILE OPTIONS on all the .acl files FILE in the directory
# DIRECTORY and in its sub-directories recursively.
# Example:
#    ./InterpretAll.sh Std/Main.acl "--verbose --input --output no-rules short-names

current_path="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
options=$2
tmp_files=$(mktemp)
tmp_errors=$(mktemp)

find "$1" -type f -name "*.acl" -print0 | sort -z | while IFS= read -r -d $'\0' file; do
    cmd="$current_path/aclove --file $file $2"
    printf "Interpreting file $file ...\n"
    eval "$cmd"
    if [ $? -ne 0 ]; then
         echo "1" >> "$tmp_errors"
    fi
    echo "1" >> "$tmp_files"
    printf "Done.\n\n"
done

nb_errors=$(wc -l < "$tmp_errors")
nb_files=$(wc -l < "$tmp_files")
rm "$tmp_errors"
printf "There are $nb_errors/$nb_files file(s) with error(s).\n"

