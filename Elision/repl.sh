#!/bin/bash

dir=`dirname $0`
val=`for file in \`find lib -name '*.jar'\` ; do \
  echo -n $file":"; \
  done`"$dir/bin"
scala \
	-i \
	-cp $val \
	sjp.elision.repl.Repl

