#!/usr/bin/env sh
#
# Build for Travis
#

echo "Found the travis.sh script, and executing."
cd  Elision
echo "Switched directory to the Elision project."
if [ ! -z $SCALA_VERSION ] ; then
  sbt ++$SCALA_VERSION clean package
else
  sbt clean package
fi
echo "Completed the build using sbt."

