type 'a cyclic_list = {
  elements: 'a array;
  mutable i: int;
  length:int;
}

type player = { 
  mutable health: int;
  mutable feed: int;
  mutable x: int;
  mutable y:int;
  texture: (Raylib.Texture.t cyclic_list) array;  
  mutable direction: int;
} 

type batiments =
{
  width:int;
  height: int;
  x:int;
  y:int;
  texture :Raylib.Texture.t
}

type map = 
{
  mutable batiment: batiments list;
  floor: Raylib.Texture.t;
  mutable roads: batiments list;
  mutable generated: (int*int) list
}


let cyclic_next c_list =
  if(c_list.i >= c_list.length -2) then 
    c_list.i <- 0
  else c_list.i <- c_list.i + 1;
  ()

let cyclic_top c_list = 
  c_list.elements.(c_list.i)  