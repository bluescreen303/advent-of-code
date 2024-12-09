#!/usr/bin/env -S jaq -L . -Rsfr

def per($n): if length >= $n then .[0:$n], (.[$n:] | per($n)) end;

# return a single occupied block, skipping empty space, or null if there are none left
# [block, blocks_read, remainder]
def take:
  if length == 0    then [null, 0, .]
  elif .[0] == null then .[1:] | take | (.[1] += 1)
  else                   [.[0], 1, .[1:]]
  end
;

split("")
| map(tonumber?)
| [per(2)]                                                 # pairs of [blocks_taken, blocks_free]
| [[range(length)], .]                                     # prepend file_id
  | transpose
  | map( . as [$id, [$size, $free]]                        # expand...
       | (range($size) | $id), (range($free) | null))      # emit file_id or null per block
| length as $len
| reduce .[] as $block
  ( { result: [], $len, rev: reverse }                     # iterate forward, while also reading backward ($rev) to fill empty blocks,
  ; (.rev | take) as [$rev_block, $rev_n, $rev_rest]       # keeping track of when forward crosses backward in $len
  | if .len <= 0 or $block == null and .len - $rev_n <= 0  # when we passed the mark (or would pass the mark by reading reverse)
    then .result += [null]                                 # emit empty block
    elif $block == null
    then .result += [$rev_block]                           # for empty blocks, emit from reverse
       | .len    -= $rev_n
       | .rev     = $rev_rest
    else .result += [$block]                               # emit filled blocks by default
    end
  | .len -= 1
  ).result
| [[range($len)], .] | transpose                           # enumerate block numbers
| reduce .[] as $item (0; . + $item[0] * ($item[1] // 0))  # calculate checksum
