#!/usr/bin/env -S jaq -L . -Rsfr

include "aoc-jq/2024/07-shared";

main( .[0] + .[1]
    , .[0] * .[1]
    )
