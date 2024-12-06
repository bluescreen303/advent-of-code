#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;
include "aoc-jq/2024/06-shared";

utils::parse_grid
| default_route
| utils::show_grid, "", ([.[][] | is_guard(.)] | length)
