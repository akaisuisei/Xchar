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
	begin
	let rotok= ref false and d_rot = ref 0 and b_show = ref false in
	let range = Array.length Sys.argv in
	for i = 0 to range -1 do
		begin
		match Sys.argv.(i) with
			| "-r" -> if i + 1 < range -1 then
	  			begin
	    			rotok := true;
	    			d_rot := int_of_string (Sys.argv.(i+1))
	  			end
			| "-s" -> b_show := true
			| _ -> ()
		end
	done;
	sdl_init ();
	let img = Sdlloader.load_image Sys.argv.(1) in
	let (w,h) = get_dims img in 
	let img1 = Pretreatment.image2gray img in
	let img2 = Pretreatment.average (img1) 165 in
	let img3 = Pretreatment.binarization (Pretreatment.average img1 140) 200 in
	let a = (fun x y b -> if b then x else y ) !d_rot (Rotation.detect_angle img3) !rotok in
let img4 = Rotation.rotate img3 a in
	let img5 = Recognition.recognition img4 in
	let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
	(fun x -> if x then
	show img display;
	wait_key ();
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
	) !b_show;

	show img5 display;
	wait_key ();
	exit 0
	end

let _ =  main ()
