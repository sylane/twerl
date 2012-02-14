#!/bin/bash

ROOT=$(cd $(dirname $0); pwd)

erl -pz $ROOT/ebin -s reloader -s erlog -s twerl -boot start_sasl "$@"
