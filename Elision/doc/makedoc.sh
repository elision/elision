#!/bin/bash
#######################################################################
#      _ _     _
#  ___| (_)___(_) ___  _ __
# / _ \ | / __| |/ _ \| '_ \
#|  __/ | \__ \ | (_) | | | |
# \___|_|_|___/_|\___/|_| |_|
#The Elision Term Rewriter
#
#Copyright (c) 2012 by UT-Battelle, LLC.
#All rights reserved.
#
#Redistribution and use in source and binary forms, with or without
#modification, are permitted provided that the following conditions are met:
#
#1. Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#2. Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
#Collection of administrative costs for redistribution of the source code or
#binary form is allowed. However, collection of a royalty or other fee in excess
#of good faith amount for cost recovery for such redistribution is prohibited.
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
#CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
#OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
#WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
#OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
#ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#######################################################################
# Figure out where this is being run.
mydir=`dirname $0`

# Change to that directory.
pushd "$mydir" >/dev/null

# Make the api folder if it does not exist.
if [ ! -e api ] ; then mkdir api ; fi
cd api

# Locate all source files.
srcdir=../../src/ornl/elision
files=`find $srcdir -name '*.scala'`

# Build the documentation.
scaladoc \
	-unchecked \
	-deprecation \
	-classpath "../../lib/parboiled-core-1.0.2.jar:../../lib/parboiled-scala-1.0.2.jar" \
	-doc-title "Elision Scala API Documentation" \
	-doc-version `date +'%Y%m%d'` \
	$files
retval=$?
	
# This is not really necessary, but we do it anyway.
popd >/dev/null

# Done!  Exit with the same return value as the scaladoc command returned.
exit $retval
