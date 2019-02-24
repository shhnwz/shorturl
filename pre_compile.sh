#!/bin/sh
rebar3 get-deps
git apply --directory='_build/default/lib/riakc/' riakc_erl20.diff
