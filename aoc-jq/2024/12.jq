#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

def all_directions:
  (.y -= 1), (.y += 1), (.x -= 1), (.x += 1) # up, down, left, right
;

def neighbours: . as $region | map(all_directions) - $region;
def perimeter: neighbours | length;
def area: length;

utils::parse_grid
| utils::with_context
| . as $grid | ($grid | length) as $height | ($grid[0] | length) as $width |

def read_grid:
  select(.y >= 0 and .y < $height and .x >= 0 and .x < $width) | $grid[.y][.x]
;

def is_grid_top: select(.y < 0);
def is_grid_bottom: select(.y == $height);
def is_grid_left: select(.x < 0);
def is_grid_right: select(.x == $width);

def region:
  . as $region |
  [ .[] | .value as $me
  | all_directions
  | read_grid
  | select(.value == $me)
  ] | unique - $region as $found

  | if $found != [] then
      $region + $found | region
    else
      $region
    end
;

def regions:
  reduce .[][] as $location
  ( []
  ; if [.[][]] | index($location) == null then
      . + [[$location] | region]
    end
  )
;


def sides:
  . as $region | .[0].value as $me |

  def is_in:  read_grid | select(.value == $me);
  def is_out: read_grid | select(.value != $me);
  def is_top_edge:         .y-=1 | is_grid_top, is_out;
  def is_bottom_edge:      .y+=1 | is_grid_bottom, is_out;
  def is_left_edge:        .x-=1 | is_grid_left, is_out;
  def is_right_edge:       .x+=1 | is_grid_right, is_out;
  def left_up_in_region:   .x-=1 | is_in | .y-=1 | is_in;
  def left_down_in_region: .x-=1 | is_in | .y+=1 | is_in;
  def up_left_in_region:   .y-=1 | is_in | .x-=1 | is_in;
  def up_right_in_region:  .y-=1 | is_in | .x+=1 | is_in;

  reduce .[] as $plot
    ( 0
    ; . + ( $plot |
           [ is_bottom_edge * is_left_edge
           , is_bottom_edge * left_down_in_region
           , is_top_edge    * is_left_edge
           , is_top_edge    * left_up_in_region
           , is_right_edge  * is_top_edge
           , is_right_edge  * up_right_in_region
           , is_left_edge   * is_top_edge
           , is_left_edge   * up_left_in_region
           ] | length)
    )
;

regions
| map(area * perimeter), map(area * sides)
| add
