(*////Easy part\\\\*)
(* Use the adequate values for threesolds!*)



let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)


let first (a,b,c)=a;;

(*                 *)
(*   Grayscale     *)
(*                 *)

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

(*                 *)
(*  Binarization   *)
(*                 *)

(* the thresold will be adapted to the picture in a near future *)
(* current thresold: between 190 and 200, 200 being quite good  *)

let binarization img thresold=
  let (w,h)= get_dims img in
  let test= Sdlvideo.create_RGB_surface_format img [] w h in
  for i=0 to w-1 do
    for j=0 to h-1 do
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
;;


(* OTSU *)
(* HISTOGRAM*)

(*                 *)
(*     Average     *)
(*     Filter      *)

(*We use a thresold to improve the accuracy and prevent part of chars to be removed*)
(* CURRENT THRESOLD: 165, DO NOT CHANGE THE VALUE PLEASE *)

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
top
;;

(* Dimensions d'une image *)

 
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
    let img = Sdlloader.load_image Sys.argv.(1) and out=Sdlloader.load_image Sys.argv.(2) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
      show img display;
      (* on attend une touche *)
      wait_key ();
    show (image2gray img) display;
    wait_key ();
    show (average (image2gray img) 165) display;
    wait_key ();
    show (binarization (average (image2gray img) 165) 200) display;
    
    wait_key ();
    Sdlvideo.save_BMP  out "picture.BMP";
    
      (* on quitte *)
      exit 0
  end
 
let _ = main ()




 
