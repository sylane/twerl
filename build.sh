#!/bin/bash

ROOT=$(cd $(dirname $0); pwd)
erlc -o $ROOT/ebin -I $ROOT/include $ROOT/src/*.erl
