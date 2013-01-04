#!/bin/sh

set -e

MEM=${MEM:-4G}

sbt stage && \
  ./target/start -XX:+TieredCompilation -XX:+UseConcMarkSweepGC -Xms$MEM -Xmx$MEM "$@"

