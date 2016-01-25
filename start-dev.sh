#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname whiplash_sse_dev \
    -s whiplash_sse \
    -s reloader
