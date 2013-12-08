let first (a,b,c)=a;;
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h);;

(*                 *)
(*     Average     *)
(*     Filter      *)

let average img thresold= 
  let (w,h)=get_dims img in
   let top= Sdlvideo.create_RGB_surface_format img [] w h in
  for x=0 to w-1 do
    for y=0 to h-1 do
      Sdlvideo.put_pixel_color top x y (Sdlvideo.get_pixel_color img x y);
    done;
  done;
  for i=1 to w-2 do
    for j=1 to h-2 do
      if (first (Sdlvideo.get_pixel_color img i j)<thresold) then
	(
      let acc= ref 0 in
      for a=i-1 to i+1 do
	for b=j-1 to j+1 do
	  acc:= !acc+ first (Sdlvideo.get_pixel_color img a b);
	done;
      done;      
      let va= !acc / 9 in
      Sdlvideo.put_pixel_color top i j (va,va,va);
	)
      
    done;
  done;
Sdlvideo.save_BMP (top) "temp2.bmp";
top
;;



let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end;;


