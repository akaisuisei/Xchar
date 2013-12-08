
(*Sdl *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h);;
let first (a,b,c)=a;;

(*Grayscale*)

let level (r,g,b)  =
         (0.30 *. float_of_int(r) +. 0.59 *. float_of_int(g) +. 0.11 *. float_of_int(b)) /. 255.


let color2gray (r,g,b)= 
  let gray=int_of_float (float_of_int r *. 0.3 +. float_of_int g *. 0.59 +. float_of_int b *. 0.11) in (gray,gray,gray);;

let image2gray img=
  let (w,h)= get_dims img in
   let test= Sdlvideo.create_RGB_surface_format img [] w h in
  for i=0 to w-1 do
    for j=0 to h-1 do
      Sdlvideo.put_pixel_color test i j ( (color2gray (Sdlvideo.get_pixel_color img i j)));
    done;
  done;
test
;;



(*Binarization*)


let binarization img thresold=
  let (w,h)= get_dims img in
  let test= Sdlvideo.create_RGB_surface_format img [] w h in
  for i=0 to w-1 do
    for j=0 to h-1 do
      if (first (Sdlvideo.get_pixel_color img i j) < thresold+25 ) then
	(
	Sdlvideo.put_pixel_color test i j (0,0,0);
	)
      else 
	(
	Sdlvideo.put_pixel_color test i j (255,255,255);
	)
    done;
  done;
Sdlvideo.save_BMP (test) "temp.bmp";
test



(*Otsu's method to binarize the picture, we also write the thresold and the variance*)

 let otsu img =
   let (width,height) = get_dims img in
   let sum = ref 0 and sumB = ref 0 in
   let wB = ref 0 and wF = ref 0 in
   let varianceMax = ref 0. and treshold = ref 0 in
   let histo = ref [||] in
   histo := Array.make 256 0;
   for x = 0 to width-1 do
     for y = 0 to height-1 do
       let a = int_of_float(255. *.(level (Sdlvideo.get_pixel_color img x y)) ) in
       !histo.(a) <- !histo.(a) + 1;
     done;
   done;
   for i = 0 to 255 do
     sum := !sum + i * !histo.(i);
   done;
   let cond = ref false in
   for i = 0 to 255 do
     if (not !cond) then
       begin
	 wB := !wB + !histo.(i);
	 if (!wB <> 0) then
	   begin
	     wF := (width*height) - !wB;
	     if (!wF = 0) then
	       begin
		 cond := true;
               end 
	     else
	       begin
		 sumB := !sumB + (i * !histo.(i));
		 let mB = (float)(!sumB/(!wB)) and mF = (float)((!sum-(!sumB))/(!wF)) in
		 let varianceB = (float) !wB *.(float) !wF *.(mB-.mF) *.(mB-.mF) in
		 treshold := if varianceB > !varianceMax then i else !treshold;
		 varianceMax := if varianceB > !varianceMax then varianceB else !varianceMax;
               end; 
           end;
       end;
   done;
  !treshold;;
 
(*...*)




let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end;



