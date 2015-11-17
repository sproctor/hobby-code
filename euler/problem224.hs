is_triple a b c =
  a * a + b * b == c * c - 1 && c >= b && b >= a

get_triangles_where_a a p =
  let
    b = (p*p - 2*a*p - 1) `div` (2*p - 2*a)
    c = p - a - b
  in if is_triple a b c
       then print (a,b,c,p)
       else return ()

get_triangles_where_p p =
  let
    aux a =
      if 3 * a > p then return ()
      else do get_triangles_where_a a p
              aux (a + 1)
  in aux 1

get_triangles start stop =
  let
    aux p =
      if p > stop then return ()
      else do get_triangles_where_p p
              aux (p + 2)
  in aux start
  
main = get_triangles 3 1000
