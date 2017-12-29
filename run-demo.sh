#!/usr/bin/env bash

pkill demo-web-service || true
demo-web-service &

pkill demo-sink || true
demo-sink &
