open Types

let inside_collision x y  (bat:Types.batiments) = 
  abs (bat.x - x + 250) >= 370 || abs (bat.y - y ) >= 300 


let rec outside_collsion x y (bat:Types.batiments list) = 
match bat with 
|t::q -> if x < t.x + t.width && x > t.x - t.width / 2  && y<t.y + t.height / 2 && y>t.y -t.height then true 
                                                                                    else outside_collsion x y q
|[] -> false

let collision x y (inside_house:Types.batiments option ) (map: Types.map)  = 
  if inside_house <> None then inside_collision x y  (Option.get (inside_house))
  else outside_collsion x y map.batiment