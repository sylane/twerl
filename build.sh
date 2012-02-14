#!/bin/bash

ROOT=$(cd $(dirname $0); pwd)

ERLC_PARAMS=( \ 
  -Wall \
  -pa $ROOT/ebin \
  -o $ROOT/ebin \
  -I $ROOT/include \
  -I $ROOT/specs )

erlc ${ERLC_PARAMS[*]} $ROOT/src/gen_*.erl

erlc ${ERLC_PARAMS[*]} $ROOT/ext/*.erl

erlc ${ERLC_PARAMS[*]} $ROOT/src/*.erl

erlc ${ERLC_PARAMS[*]} $ROOT//test/*.erl
