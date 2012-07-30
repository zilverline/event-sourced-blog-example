#!/bin/bash

SBT_OPTS="-XX:+UseParallelGC -XX:+TieredCompilation -Xms4G -Xmx4G -Dlogback.configurationFile=conf/logback-performance.xml" sbt "test:run-main performance.PerformanceTest $*"
