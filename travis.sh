#!/usr/bin/env sh
#
# Build for Travis
#

echo "Found the travis.sh script, and executing."
cd  $CI_HOME/Elision
echo "Switched directory to the Elision project."
sbt ++$TRAVIS_SCALA_VERSION package
echo "Completed the build using sbt."

