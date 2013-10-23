(*////Easy part\\\\*)
(* Use the adequate values for threesolds!*)


let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let first (a,b,c) = a

(*                 *)
(*   Grayscale     *)
(*                 *)

let color2gray (r,g,b) = 
  let gray  =  int_of_float (float_of_int r *. 0.3 +. float_of_int g *. 0.59 +. float_of_int b *. 0.11) in
(gray,gray,gray)

let image2gray img =
  let (w,h) = get_dims img in
   let test = Sdlvideo.create_RGB_surface_format img [] w h in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      Sdlvideo.put_pixel_color test i j ( (color2gray (Sdlvideo.get_pixel_color img i j)));
    done;
  done;
test

(*                 *)
(*  Binarization   *)
(*                 *)

(* the thresold will be adapted to the picture in a near future *)
(* current thresold: between 190 and 200, 200 being quite good  *)

let binarization img thresold =
  let (w,h) = get_dims img in
  let test = Sdlvideo.create_RGB_surface_format img [] w h in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      if (first (Sdlvideo.get_pixel_color img i j) < thresold ) then
	(
	Sdlvideo.put_pixel_color test i j (0,0,0);
	)
      else 
	(
	Sdlvideo.put_pixel_color test i j (255,255,255);
	)
    done;
  done;
test

(*                 *)
(*     Average     *)
(*     Filter      *)

(*We use a thresold to improve the accuracy and prevent part of chars to be removed*)
(* CURRENT THRESOLD: 165, DO NOT CHANGE THE VALUE PLEASE *)

let average img thresold =  
  let (w,h) = get_dims img in
   let top =  Sdlvideo.create_RGB_surface_format img [] w h in
  for x = 0 to w-1 do
    for y = 0 to h-1 do
      Sdlvideo.put_pixel_color top x y (Sdlvideo.get_pixel_color img x y);
    done;
  done;
  for i = 1 to w-2 do
    for j = 1 to h-2 do
      if (first (Sdlvideo.get_pixel_color img i j)<thresold) then
	(
      let acc =  ref 0 in
      for a = i-1 to i+1 do
	for b = j-1 to j+1 do
	  acc:=  !acc+ first (Sdlvideo.get_pixel_color img a b);
	done;
      done;      
      let va =  !acc / 9 in
      Sdlvideo.put_pixel_color top i j (va,va,va);
	)
    done;
  done;
top
