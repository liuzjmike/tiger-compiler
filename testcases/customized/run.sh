#!/bin/bash

cat ../runtimele.s ../sysspim.s $1 > "linked_$1"
spim -f "linked_$1"
