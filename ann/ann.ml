class ann (nb1:int) (nb2:int) =
object (self)
  val alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  val epoch = 200
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
	  input.(i*h+j) <- 1.;
      done;
    done;
    input

  method rescale img w2 h2 =
    let w = (Sdlvideo.surface_info img).Sdlvideo.w
    and h = (Sdlvideo.surface_info img).Sdlvideo.h in
    let input = Array.make (w2*h2) 0. in
    let (x_ratio, y_ratio) = ((float_of_int w /. float_of_int w2), (float_of_int h /. float_of_int h2)) in
    for i = 0 to w2-1 do
      for j = 0 to h2-1 do
        let (px,py) = ((int_of_float ((float_of_int i) *. x_ratio)), (int_of_float ((float_of_int j) *. y_ratio))) in
        let (a,b,c) = Sdlvideo.get_pixel_color img px py in
        if ((a+b+c)/3) > 128 then
          input.(i*h2 + j) <- 1.;
      done;
    done;
    input


  method get_max a =
    let i = ref 0.
    and sol = ref 0 in
    for j = 0 to Array.length a -1 do
      if !i< a.(j) then
        begin
          i := a.(j);
          sol := j
        end;
    done;
    sol


  method learn_alphabet fname =
    Sdlttf.init ();
    print_int (String.length alphabet);
    let font = self#open_font fname in
    let l = String.length alphabet in
    let inputs = Array.make_matrix l (7*9) 0.
    and targets = Array.make_matrix l l 0. in
    for i = 0 to l-1 do
      let img = Sdlttf.render_glyph_solid font (alphabet.[i]) Sdlvideo.black in
      let w = (Sdlvideo.surface_info img).Sdlvideo.w
      and h = (Sdlvideo.surface_info img).Sdlvideo.h in
      targets.(i).(i) <- 1.;
      if (w <> 7) || (h<> 9) then
        inputs.(i) <- self#rescale img 7 9
      else
        inputs.(i) <- self#img_to_input img
    done;
    self#training inputs targets;
    inputs

  method test_result a =
    for i = 0 to Array.length a -1 do
      inputs <- a.(i);
      self#forward_propagation;
      for j = 0 to nb_sorties -1 do

        if outputs.(j) > 0.4 then
          begin
            print_int i;
            print_string " ";
            print_int j;
            print_string " ";
            print_float outputs.(j);
            print_newline ();
          end
        else
          if (i=j) then
            begin
              print_string "fail";
              print_newline ()
            end

      done;
    done;

  method recognize img =
    let w = (Sdlvideo.surface_info img).Sdlvideo.w
    and h = (Sdlvideo.surface_info img).Sdlvideo.h in
    if (w = 7) && (h= 9) then
      inputs <- self#img_to_input img
    else
      inputs <- self#rescale img 7 9;
    self#forward_propagation;
    self#get_max outputs;

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



  method update_weights error delta =
    for i = 0 to Array.length weights -1 do
      for j = 0 to Array.length delta.(0) -1 do
	weights.(i).(j+1) <- weights.(i).(j+1) +.(learning_rate *. delta.(i).(j))
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
        self#update_weights error delta;
      done;

    done;

  method save fname =
    let f = open_out_bin fname in
    let g x = Marshal.to_channel f x [Marshal.Closures] in
    g nb_entrees;
    g nb_sorties;
    g inputs;
    g outputs;
    g weights;
    close_out f

  method load fname =
    let f = open_in_bin fname in
    nb_entrees <- Marshal.from_channel f;
    nb_sorties <- Marshal.from_channel f;
    inputs <- Marshal.from_channel f;
    outputs <- Marshal.from_channel f;
    weights <- Marshal.from_channel f;
    close_in f



  initializer self#randominit

end
