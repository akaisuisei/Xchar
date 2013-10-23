let get_dims img =
	((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

(* init de SDL *)
let sdl_init () =
	begin
	Sdl.init [`EVERYTHING];
	Sdlevent.enable_events Sdlevent.all_events_mask;
	end

(* attendre une touche ... *)
let rec wait_key () =
	let e = Sdlevent.wait_event () in
	match e with
		Sdlevent.KEYDOWN _ -> ()
		| _ -> wait_key ()

(*
show img dst
affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
	let d = Sdlvideo.display_format img in
	Sdlvideo.blit_surface d dst ();
	Sdlvideo.flip dst


let main ()=
	let diver = ref 0 and a = ref 0 in
	let rotok= ref false and d_rot = ref 0 and b_show = ref false in
	let range = Array.length Sys.argv in
	for i = 1 to range do
		match Sys.argv.(i) with
			| "-r" -> if i + 1 < range then
	  			begin
	    			rotok := true;
	    			a := int_of_string ( Sys.argv.(i+1) );
	  			end
			| "-s" -> b_show := true;
			| _ -> diver := 1; 
	done;
	sdl_init();
	let img = Sdlloader.load_image Sys.argv.(1) in
	let (w,h) = get_dims img in 
	let img1 = Pretreatment.image2gray img in
	let img2 = Pretreatment.average ( img1) 165 in
	let img3 = Pretreatment.binarization (Pretreatment.average img1 140) 200 in
	if not !rotok then
	begin
		a := Rotation.detect_angle img3 ;
	end
	let img4 = Rotation.rotate img3 !a in
	let img5 = Recognition.recognition img4 in
	let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
	show img display;
	wait_key ();
	if !b_show then
		begin
		show img1 display;
		wait_key ();
		show img2 display;
		wait_key ();
		show img3 display;
		wait_key ();
		show img4 display;
		wait_key ();
		show img5 display;
		wait_key ();
		end

	show img5 display;
	wait_key ();
	exit 0

let _ =  main ()
