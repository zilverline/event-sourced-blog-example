#!/bin/bash

MEM=${MEM:-256m}

SBT_OPTS="-XX:+UseParallelGC -XX:+TieredCompilation -Xms$MEM -Xmx$MEM -Dlogback.configurationFile=conf/logback-performance.xml" sbt "test:run-main performance.PerformanceTest $*"
