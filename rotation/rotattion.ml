(* Dimensions d'une image *)
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
(*
let rotate img angle =
	Sdlgfx.rotozoomSurface img angle 1. false

let max matrix=
let (maxi,mtheta,mrot)= (0,0,0) in
for theta= 0 to 180 do
	for rot=0 to 180 do
		if maxi < matrix.(theta).(rot) then
			mrot<-rot
	done
done 
matrix.(mtheta).(mrot)
*)
let detect_angle img =
	let (w,h)=get_dims img in
	let (w2,h2)= (w/2,h/2) in
	let pi =4. *.(atan 1.) in
	let larg= (1+int_of_float(sqrt(float_of_int (w*w + h*h)))) in
	let vote=  (Array.make_matrix 181 larg 0) in
	for x=0 to w do
		for y=0 to h do
			let (r,g,b) =Sdlvideo.get_pixel_color img x y in
			if r<>0 then begin
				let maxRot= sqrt (float_of_int (w2*w2+h2*h2)) in
				let (xspec,yspec)=(float_of_int (x-w2),float_of_int (y-h2)) in
				for alpha = 0 to 180 do
					let th = (float_of_int alpha)*.pi/.180. in
					let rot = xspec*.(cos th) +. yspec *.(sin th) in
					let i_theta = int_of_float (th/.(2.*.pi)*.180.) in
					let i_rot = int_of_float (((1.+.rot/.maxRot)/.2.)*. float_of_int larg) in
					vote.(i_theta).(i_rot) <- vote.(i_theta).(i_rot) +1
				done
				end
		done
	done
