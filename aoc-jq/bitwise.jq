module {
  "name": "bitwise",
  "description": "bit arrays, bit streams, and integers",
  "version": "2024.04.06",
  "homepage": "https://rosettacode.org/w/index.php?title=Category:Jq/bitwise.jq",
  "license": "MIT",
  "author": "pkoppstein at gmail dot com",
};

# This jq module defines functions for bit-wise operations on integers,
# together with an integer-divsion function, div/1, which is defined
# to take advantage of gojq's support for ininite-precision integer arithmetic.

# To take advantage of gojq's infinite-precision integer arithmetic:
# if the input and $j are integers, then the result will be an integer.
def div($j):
  (. - (. % $j)) / $j | floor;

# Convert the input integer to a stream of 0s and 1s, least significant bit first
def bitwise:
  recurse( if . >= 2 then div(2) else empty end) | . % 2;

# Input is normally an integer but could be a number
def rightshift($n):
  reduce range(0;$n) as $i (.; div(2));

# Input is normally an integer but could be a number
def leftshift($n):
  reduce range(0;$n) as $i (.; . * 2);

# Flip 0 and 1 and leave everything else as is
def flipbits(stream):
  stream | if . == 0 then 1 elif . == 1 then 0 else . end;

# The inverse of `bitwise`
def stream_to_integer(stream):
  reduce stream as $c ( {power:1 , ans: 0};
      .ans += ($c * .power) | .power *= 2 )
  | .ans | floor;

# If the input is a number, emit its round value (because jaq uses `round` to convert real to integer);
# otherwise, the input should be a a bit-array interpreted as having the least-significant bit first
def to_int:
  if type == "number" then round
  else stream_to_integer(.[])
  end ;

# If $n is null, then the bitwise bits of the input integer are simply flipped;
# otherwise, $n, which should be non-negative, determines the width for padding or truncation
def flip($n):
  if $n == null then stream_to_integer(flipbits(bitwise))
  else [limit($n; bitwise)] as $bits
  | stream_to_integer(flipbits( $bits[], (range(0; $n - ($bits|length)) | 0)))
  end;

# Emit an array of the $n least-significant bits of the input non-negative integer;
# the length of the array will thus be less than or equal to $n
def lsbits($n): [limit($n; bitwise)];

# $x and $y should be two non-negative integers or arrays as produced by [bitwise].
# Emit the integer corresponding to the bitwise-or of $x and $y
def bitwise_or($x;$y):
   def tobitarray: if type == "number" then [bitwise] else . end;
   def lor($a;$b):
     if $a==1 or $b==1 then 1
     else 0
     end;
   if   $x == 0 or $x == [0] then $y | to_int
   elif $y == 0 or $y == [0] then $x | to_int
   else ($x|tobitarray) as $s
   | ($y|tobitarray) as $t
   | stream_to_integer(
       range(0; [($s|length), ($t|length)] | max) as $i
       | lor($s[$i]; $t[$i]) )
   end;

# Emit the integer corresponding to `$x bitwise-and $y`,
# assuming these are non-negative integers or bitarrays
def bitwise_xor($x;$y):
   def tobitarray: if type == "number" then [bitwise] else . end;
   def lxor($a; $b):
     if ($a==1 or $b==1) and (($a==1 and $b==1)|not) then 1
     elif $a == null then $b
     elif $b == null then $a
     else 0
     end;
   if $x == 0 or $x == [0] then $y | to_int
   elif $y == 0 or $y == [0] then $x | to_int
   else
     ($x|tobitarray) as $s
   | ($y|tobitarray) as $t
   | stream_to_integer(
       range(0; [($s|length), ($t|length)] | max) as $i
       | lxor($s[$i]; $t[$i]) )
   end ;

# Emit the integer corresponding to `$x bitwise-and $y`,
# assuming these are non-negative integers or bitarrays
def bitwise_and($x; $y):
   def tobitarray: if type == "number" then [bitwise] else . end;
   def land($a;$b):
      if ($a==1 and $b==1) then 1 else 0 end;

   if $x == 0 or $y == 0 or $x == [0] or $y == [0] then 0
   else ($x|tobitarray) as $s
   | ($y|tobitarray) as $t
   | stream_to_integer(
       range(0; [($s|length), ($t|length)] | min) as $i
       | land($s[$i]; $t[$i]) )
   end ;

# A special case of $x xor $y where both are non-negative integers
# If the input is a number, it determines the maximum number of bits to consider
def xor($x;$y):
  (if type == "number" then round else null end) as $n
  | if $n == null then bitwise_xor($x;$y)
    else bitwise_xor( $x | lsbits($n); $y | lsbits($n) )
    end ;
