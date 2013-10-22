
let print_input inputs = 
  print_string "Inputs : ";
  print_newline ();
  for i = 0 to Array.length inputs -1 do
    for j = 0 to Array.length inputs.(0) -1 do
      print_float inputs.(i).(j);
      print_string " | ";
    done;
    print_newline ();
  done

let print_hopes outputs = 
  print_string "Hopes : ";
  print_newline ();
  for i = 0 to Array.length outputs -1 do
    for j = 0 to Array.length outputs.(0) -1 do
      print_float outputs.(i).(j);
      print_string " | ";
    done;
    print_newline ();
  done

	

let main () = 
  begin
    let p = new ANN.ann 2 2 1
    and train = new Training.training ()
    and inputs = Array.make_matrix 4 2 1.
    and outputs = Array.make_matrix 4 1 1.
    in
    inputs.(1).(1) <- 0.;
    inputs.(2).(0) <- 0.;
    inputs.(3).(0) <- 0.;
    inputs.(3).(1) <- 0.;
    outputs.(1).(0) <- 0.;
    outputs.(2).(0) <- 0.;
    print_input inputs;
    print_hopes outputs;
    print_string "Neural Network is learning...";
    train#all p inputs outputs;
    print_newline ();
    print_string "Neural Network's answer for each case : ";
    print_newline ();
    for q = 0 to Array.length inputs - 1 do
      p#forward_propagation inputs.(q) (p#get 1) (p#get 2);
      p#get_result;
    done;
  end


let _ = main ()
