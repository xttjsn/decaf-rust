#!/bin/bash
FILES=./examples/*

for f in $FILES
do
	echo "Compiling $f..."
	./target/debug/decafc "$f" 1>/dev/null 2>/dev/null || exit;
done
