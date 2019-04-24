#!/bin/bash

cat runtimele.s sysspim.s $1 > "new_$1"
spim -f "new_$1"