let pi = 4. *. (atan 1.) 
(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
let maxi matrix=
	let maxis = ref 0 and m_i = ref 0 and  m_j = ref 0 in
	for i = 0 to Array.length matrix -1 do
	for j = 0 to Array.length matrix.(0) -1 do
	if !maxis <= matrix.(i).(j) then
			begin	maxis := matrix.(i).(j);
				m_i := i;
				m_j := j;
			end
		done;
	done;
	(!maxis,!m_i,!m_j)

let detect_angle img =
	let (w,h) = get_dims img in
	let (w2,h2) = (w/2,h/2) and larg = 1 + int_of_float (sqrt (float_of_int (w*w + h*h) )) / 2 in
	let vote = (Array.make_matrix 180 larg 0) in
	for x = 0 to w do
		for y = 0 to h do
			let (r,g,b) = Sdlvideo.get_pixel_color img x y in
			if r = 0 then
				begin
				let maxRot = sqrt (float_of_int (w2 * w2 + h2 * h2)/.2.) in
				let (xspec,yspec) = (float_of_int (x-w2 ),float_of_int (y-h2)) in
				for alpha = 0 to 179 do
					let th = (float_of_int alpha) *. pi /. 180. in
					let rot =(xspec *. (cos th) +. yspec *. (sin th)) in
					if rot >= 0. then 
						begin
						let x2= (x+10)*cos th + (y+10)*sin th and
							y2 =(x+10)*sin th +(y+10)
						let i_rot = int_of_float ((( rot /. maxRot) /. 2.) *. float_of_int larg )  in
						vote.(alpha).(i_rot) <- vote.(alpha).(i_rot) +1;
						end
				done;
				end
		done;
	done;
	Printf.printf("t");
	let (_,a,_) = maxi vote in	
a

let rotate img angle =
	let (w,h) = get_dims img and a = (float_of_int angle) *. pi /. 180. in
	let (cosA,sinA) = (cos a,sin a ) in
	let n_w = int_of_float ((float_of_int w *. cosA) +. ((float_of_int h) *. sinA)) and
		n_h = int_of_float ((float_of_int h) *. cosA +. (float_of_int w) *. sinA ) in
	let img2 = Sdlvideo.create_RGB_surface_format img [] n_w n_h in
	let (c_x,c_y) = (w/2,h/2) and (cn_x,cn_y) = (n_w/2,n_h/2) in

	for x = 0 to n_w do
		for y = 0 to n_h do
			let (posx,posy) = ((float_of_int x -. float_of_int cn_x),
				(float_of_int y -. float_of_int cn_y)) in
			let testx = int_of_float ( cosA *. posx +. sinA *. posy) + c_x and
			testy = int_of_float ( cosA *. posy -. sinA *. posx) + c_y in
			if (testx < w && testy < h && testx >= 0 && testy >= 0) then
				let (r,g,b) = Sdlvideo.get_pixel_color img testx testy in
				Sdlvideo.put_pixel_color img2 x y (r,g,b);			
			else
				Sdlvideo.put_pixel_color img2 x y (255,255,255);
		done
	done;
img2

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
 
(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
	let a = detect_angle img in
Printf.printf("rr");
	let img2 = rotate img a in
	let (w,h) = get_dims img2 in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
      show img display;
      (* on attend une touche *)
      wait_key ();
	show img2 display;
	wait_key ();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()
	
