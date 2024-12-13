#!/usr/bin/env -S jaq -L . -Rsfr

# turn input into
# { files: [{pos, len, id}]  pos is the absolute block position on disk
#   free[len]: [pos]         free slots grouped by length
# }
def parse:
  split("")
  | map(tonumber?)
  | [[range(length)], .] | transpose
  | reduce .[] as [$id, $len]
      ( {pos: 0, files: [], free: [null, (range(9) | [])]}
      ; if $id % 2 == 0 then
          .files += [{pos, $len, id: $id / 2 | floor}]
        elif $len > 0 then
          .free[$len] += [.pos]
        end
      | .pos += $len
      )
;

# for puzzle a
def toblocks:                # break file blocks to 1-block
  .pos += range(.len) | .len = 1
;

def find_free($len):
  [ [ .[range($len; 10)][0] ]# check the first free position for slots of file size and up
  , [ range($len; 10) ]      # include the found length of the free slot
  ]
  | transpose
  | map(select(.[0]))        # only report results with at least one free slot
  | sort[0]                  # pick the lowest (leftmost)
  | {pos: .[0], len: .[1]}
;

def checksum:
  reduce .[] as $file
  ( 0
  ; . + $file.id             # diagonal formula
      * (2*$file.pos + $file.len-1)
      * $file.len / 2 | floor
  )
;

def defrag:
  reduce range(.files | length - 1; -1; -1) as $id
  ( .
  ; .files[$id] as $file
  | (.free | find_free($file.len)) as $free
  | if $free.pos and $free.pos < $file.pos then
      .files[$id].pos = $free.pos
    | .free |= ( if $free.len > $file.len then
                   # TODO: sort can be made faster by maintaining order (use bsearch)
                   # or implement a heap (array based)
                   .[$free.len - $file.len] |= (([$free.pos + $file.len] + .) | sort)
                 end
               | del(.[$free.len][0])
               )
    end
);

parse
| (.files[] |= toblocks, .)  # puzzle a, puzzle b
| defrag.files
| checksum
