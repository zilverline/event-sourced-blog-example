#!/bin/sh

set -e

MEM=${MEM:-256m}

sbt stage && \
  ./target/start -XX:+TieredCompilation -XX:+UseConcMarkSweepGC -Xms$MEM -Xmx$MEM "$@"

