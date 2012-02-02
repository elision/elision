#!/bin/bash

if [ ! -e api ] ; then mkdir api ; fi
pushd api
srcdir=../../src/sjp/elision
files=`find $srcdir -name '*.scala'`
scaladoc \
	-deprecation \
	-classpath "../../lib/parboiled-core-1.0.2.jar:../../lib/parboiled-scala-1.0.2.jar" \
	-doc-title "Elision" \
	-doc-version "0.0" \
	$files
popd

