#!/bin/bash

# Author: Samuele Giraudo
# Creation: oct. 2022
# Modifications: oct. 2022

cat $1 | sed -e 's/\x1b\[[0-9;]*m//g'

