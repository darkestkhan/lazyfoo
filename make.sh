#!/usr/bin/env bash

# Build the tutorials 
cd src
gprbuild tut01.gpr
gprbuild tut02.gpr
gprbuild tut03.gpr
gprbuild tut04.gpr
gprbuild tut05.gpr
cd ..
