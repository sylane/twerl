#!/bin/bash

ROOT=$(cd $(dirname $0); cd ..; pwd)

erl -noshell -run edoc_run application "'twerl'" "\"$ROOT\"" '[]'
