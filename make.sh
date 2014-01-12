#!/usr/bin/env bash

# Build the tutorials 
cd src
gprbuild tut01.gpr
gprbuild tut02.gpr
gprbuild tut03.gpr
gprbuild tut04.gpr
gprbuild tut05.gpr
gprbuild tut06.gpr
gprbuild tut07.gpr
cd ..
