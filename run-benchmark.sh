#!/bin/bash

MEM=${MEM:-4G}

SBT_OPTS="-XX:+UseParallelGC -XX:+TieredCompilation -Xms$MEM -Xmx$MEM -Dlogback.configurationFile=conf/logback-performance.xml" sbt "test:run-main performance.PerformanceTest $*"
