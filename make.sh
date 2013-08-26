#!/usr/bin/env bash

# Build the tests
cd src
gprbuild tut01.gpr
gprbuild tut02.gpr
gprbuild tut03.gpr
gprbuild tut04.gpr
cd ..
