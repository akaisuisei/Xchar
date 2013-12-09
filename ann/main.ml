let main () =
  begin
    Sdlttf.init ();
    let p = new Ann.ann (7*9) 62 in
    p#load "learned_fonts/arial";


    (*t inputs = Array.make_matrix 4 2 0.
    and outputs = Array.make_matrix 4 1 0. in
    inputs.(1).(0) <- 1.;
    inputs.(2).(1) <- 1.;
    inputs.(3).(0) <- 1.;
    inputs.(3).(1) <- 1.;
    outputs.(0).(0) <- 1.;
    p#randominit;
    print_newline ();
    p#print_w;
    print_newline ();
    p#training inputs outputs;
    for i = 0 to Array.length inputs -1 do
      p#chg_inputs (inputs.(i));
      p#forward_propagation;
      p#get_result;
      print_newline ();
    done;
   p#print_w;*)
  end

let _ = main ();
