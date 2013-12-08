class ann (nb1:int) (nb2:int) =
object (self)
  val alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val epoch = 500
  val momentum = 0.9
  val learning_rate = 0.1
  val mutable nb_entrees = nb1
  val mutable nb_sorties = nb2 
  val mutable inputs = Array.make nb1 0.
  val mutable outputs = Array.make nb2 0.
  val mutable weights = Array.make_matrix nb2 (nb1+1) 0.

  method open_font fname =
    let font = Sdlttf.open_font fname 12 in
    font;

  method img_to_input img = 
    let w = (Sdlvideo.surface_info img).Sdlvideo.w
    and h = (Sdlvideo.surface_info img).Sdlvideo.h in
    let input = Array.make (w*h) 0. in
    for i = 0 to w-1 do
      for j = 0 to h-1 do
	let (a,b,c) = Sdlvideo.get_pixel_color img i j in
	if ((a+b+c)/3) > 128 then
	  input.(i*h+h) <- 1.;
      done;
    done;
    input

  

  method randominit = 
    Random.self_init ();
    for i = 0 to Array.length weights -1 do
      for j = 0 to Array.length weights.(0) -1 do
	weights.(i).(j) <- Random.float (1.) -. 0.5
      done;
    done;

  method sigmoid x = 
    let a = exp ((-1.) *. x) in
    1./.(1. +. a)

  method forward_propagation =
    for i = 0 to Array.length outputs -1 do
      let sol = ref 0. in
      for j = 0 to Array.length inputs -1 do
	sol := !sol +. (weights.(i).(j+1) *. inputs.(j))
      done;
      sol := !sol +. weights.(i).(0);
      outputs.(i) <- (self#sigmoid !sol);
    done;

  method calc a b =
    let sol = Array.make_matrix (Array.length a) (Array.length b) 0. in
    for i = 0 to Array.length a -1 do
      for j = 0 to Array.length b -1 do
	sol.(i).(j) <- a.(i) *. b.(j)
      done;
    done;
    sol


  method first_error targets = 
    let sol = Array.make nb_sorties 0. in
    for i = 0 to nb_sorties -1 do
      sol.(i) <- ((targets.(i) -. outputs.(i)))
    done;
    sol

  method sum_mat (a:float array array) (b:float array array) = 
    let sol = Array.make_matrix (Array.length a) (Array.length b.(0)) 0. in
    for i = 0 to Array.length sol -1 do
      for j = 0 to Array.length sol.(0) -1 do
	sol.(i).(j) <- a.(i).(j) +. b.(i).(j)
      done;
    done;
    sol

  method sum_array a b = 
    let sol = Array.make (Array.length a) 0. in
    for i = 0 to Array.length sol -1 do
      sol.(i) <- a.(i) +. b.(i)
    done;
    sol


  method back_propagation error =
    let sol = self#calc error inputs in
    sol;


    
  method update_weights error delta old_weight = 
    for i = 0 to Array.length weights -1 do
      for j = 0 to Array.length delta.(0) -1 do
	weights.(i).(j+1) <- weights.(i).(j+1) +.(learning_rate *. delta.(i).(j)+. momentum *. old_weight.(i).(j+1))
      done;
    weights.(i).(0) <-weights.(i).(0) +.  (learning_rate *.error.(i));
    done;

  method get_result = 
    for i = 0 to nb_sorties -1 do
      print_float outputs.(i);
      print_newline ();
    done;
    print_newline ();

  method print_w =
    for i = 0 to Array.length weights-1 do
      for j = 0 to Array.length weights.(0) -1 do
	print_float weights.(i).(j);
	print_newline ();
      done;
    done;

  method chg_inputs a = 
    inputs <- a

  method training _inputs (targets: float array array) = 
    let old_weights = ref weights in 
    for i = 0 to epoch do 
      let _delta =ref (Array.make_matrix (Array.length weights) (Array.length weights.(0)-1) 0.)
      and _error =ref  (Array.make nb_sorties 0.) in
      for j = 0 to Array.length _inputs -1 do 
	inputs <- _inputs.(j);
	self#forward_propagation;
	let error = self#first_error targets.(j) in
	let delta = self#back_propagation error in 
	_delta := self#sum_mat (!_delta) delta;
	_error := self#sum_array (!_error) error;
      done;
      let transit = weights in 
      self#update_weights !_error !_delta !old_weights;
      old_weights := transit;
      _delta := Array.make_matrix (Array.length weights) (Array.length weights.(0)-1) 0.;
      _error := Array.make nb_sorties 0.;
    done;
    
  initializer self#randominit    

end



