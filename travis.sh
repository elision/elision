#!/usr/bin/env sh
#
# Build for Travis
#

cd  $CI_HOME/Elision
sbt ++$TRAVIS_SCALA_VERSION package

