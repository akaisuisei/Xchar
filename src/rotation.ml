
let pi = 4. *. (atan 1.) 
(* Dimensions d'une image *)
let get_dims img =
	((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let get_red img x y= let (r,b,g) = Sdlvideo.get_pixel_color img x y in r

let maximum x y z = max y (max y z) 
let rec point_transform img maxx max_y w h  =
	let x1= ref maxx and y1 = ref max_y and
	r = get_red img maxx max_y in
	if  maxx + 1 < w && max_y + 1 < h && r = 0 then
		Sdlvideo.put_pixel_color img maxx max_y (255,255,255);
		let (x,y) = point_transform img (maxx + 1) max_y w h and
		(x2,y2) = point_transform img maxx (max_y + 1) w h in
		y1 := maximum y  y2 max_y;
		x1 := maximum x  x2 maxx;
	(!x1,!y1)

let img_transform img =
	let (w,h) = get_dims img in
	for x = 0 to (w - 1) do
		for y =0 to (h - 1) do
			let r = get_red img x y in
			if r = 0 then
				let (max_x,max_y) = point_transform img x y w h in
				let (x2,y2) = ((max_x + x) / 2 ,(max_y + y) / 2) in
				Sdlvideo.put_pixel_color img x2 y2 (100,100,100)
		done
	done;
	img
	
let maxi matrix=
	let maxis = ref 0 and m_i = ref 0 and  m_j = ref 0 in
	for i = 0 to Array.length matrix -1 do
		for j = 0 to Array.length matrix.(0) -1 do
		if !maxis <= matrix.(i).(j) then
			begin
			maxis := matrix.(i).(j);
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
			if r = 100  then
				begin
				let maxRot = sqrt (float_of_int (w2 * w2 + h2 * h2)/.2.) in
				let (xspec,yspec) = (float_of_int (x-w2 ),float_of_int (y-h2)) in
				for alpha = 0 to 179 do
					let th = (float_of_int alpha) *. pi /. 180. in
					let rot =(xspec *. (cos th) +. yspec *. (sin th)) in
					if rot >= 0. then 
						begin
						let i_rot = int_of_float ((( rot /. maxRot) /. 2.) *. float_of_int larg )  in
						vote.(alpha).(i_rot) <- vote.(alpha).(i_rot) +1;
						end
				done;
				end
		done;
	done;
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

let rotate_all img angle =
	let img2 = img_transform img in
	let a = detect_angle img2 in
	rotate img a 
