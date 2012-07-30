#!/bin/sh

set -e

sbt stage && \
  ./target/start -XX:+TieredCompilation -XX:+UseConcMarkSweepGC -Xms4G -Xmx4G "$@"

