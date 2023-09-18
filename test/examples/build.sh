#!/usr/bin/env bash

PARAMS="-g -d classes"

mkdir -p classes
mkdir -p expected

javac $PARAMS src/Outer.java
javac $PARAMS src/Calls.java
