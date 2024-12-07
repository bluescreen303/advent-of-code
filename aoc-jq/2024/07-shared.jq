def parse:
  split("\n")
  | map( select(. != "")
       | split("[\\s:]+"; "")
       | map(tonumber)
       | { value: .[0], components: .[1:]}
       )
;

def valid(ops):
  .value as $val
  | any( .components
       | reduce .[1:][] as $item
           ( [.[0]]
           ; [.[] | [., $item] | ops] | unique
           )[]
       ; . == $val)
;

def main(ops):
  parse
  | map(select(valid(ops)).value)
  | add
;
