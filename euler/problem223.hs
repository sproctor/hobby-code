is_triple :: Integer -> Integer -> Integer -> Bool
is_triple a b c =
  a * a + b * b == c * c + 1

get_count_where_a :: Integer -> Integer -> Integer
get_count_where_a a p =
  let
    b = quot (p - a - quot (a * a - 1) (p - a)) 2
    c = p - b - a
  in
    if b >= a && c >= b && is_triple a b c then 1
    else 0

get_count_where_p :: Integer -> Integer
get_count_where_p p =
  let
    aux :: Integer -> Integer -> Integer
    aux a acc =
      if a > quot p 3 then acc
      else aux (a + 1) (acc + get_count_where_a a p)
  in aux 1 0

get_count :: Integer -> Integer -> Integer
get_count start stop =
  let
    aux p acc =
      if p > stop then acc
      else aux (p + 1) (acc + get_count_where_p p)
  in aux start 0
  
main = print (get_count 4 25000)
