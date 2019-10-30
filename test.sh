#!/bin/bash
FILES=./examples/*

for f in $FILES

do
	echo "Compiling $f..."

	cargo run < "$f" 1> /dev/null || exit;

done
