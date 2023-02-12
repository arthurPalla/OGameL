open Types
let rec collision x y bat = 
  match bat with 
   |t::q -> if x < t.x + t.width && x > t.x - t.width / 2  && y<t.y + t.height / 2 && y>t.y -t.height then true else collision x y q
  |[] -> false