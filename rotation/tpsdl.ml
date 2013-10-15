(*Sdlloader\.load_image arg Sdlvideo.put_pixel_color dsl x y couleur *)

let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
 
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()
 
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let is_in_bound (x,y) (w,h)=
	0<=x && x<w && 0<=y && y<h ;

let copie_pixel (x,y) (nx,ny) img1 img2=
	Sdlvideo.put_pixel_color img2 nx ny Sdlvideo.get_pixel_color img1 x y;

let set_pixel_white (x,y) img=
        Sdlvideo.put_pixel_color img x y (255,255,255);

let rotate img angle=
	let (w,h)=get_dim img in
	let (cx1,cy1)=(w/2,h/2) in
	let (nw,nh)=(w*cos angle + h*sin angle,h*cos angle + w*sin angle) in
	let (cx2,cy2)=(nw/2,nh/2) in
	let testx =0 in
	let testy =0 and
	img2 = Sdlvideo.create_RGB_surface_format img [] nw nh in
	for x=0 to (w) do
		for y=0 to (h) do
			(testx,testy) = (cos (x-cx2) + sin (y-cy2) + cx ,sin (y-cy2) + cos (x-cx2) + cy);
			if is_in_bound (testx,testy) (nw,nh) then
				copie_pixel (x,y) (testx,testy) img img2
			else
				set_pixel_white (testx,testy) img2
		done
	done
	img2

let main () =
  begin
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    sdl_init ();
    let img = Sdlloader.load_image Sys.argv.(1) in
	let img2 = rotate img 0 in
    let (w,h) = get_dims img2 in
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      show img display;
      wait_key ();
      exit 0
  end
 
let _ = main ()
