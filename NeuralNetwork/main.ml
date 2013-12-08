let main () =
  begin
    let p = new ANN.ann 2 2 1
    and train = new Training.training ()
    and inputs = Array.make_matrix 4 2 0.
    and outputs = Array.make_matrix 4 1 0.
    in
    inputs.(1).(0)<- 1.;
    inputs.(2).(1) <-1.;
    inputs.(3).(0) <- 1.;
    inputs.(3).(1) <- 1.;
    outputs.(1).(0) <- 1.;
    outputs.(2).(0) <- 1.;
    for j = 0 to Array.length inputs - 1 do
      p#forward_propagation inputs.(j) (p#get 1) (p#get 2);
      p#get_result;
    done;
    print_newline ();
    train#all p inputs outputs;
    for i = 0 to Array.length inputs -1 do 
      p#forward_propagation inputs.(i) (p#get 1) (p#get 2);
      p#get_result;
    done;
    p#saveToFile "prout";
    p#loadFromFile "prout";
    for z = 0 to Array.length inputs -1 do
      p#forward_propagation inputs.(z) (p#get 1) (p#get 2);
      p#get_result;
    done;
  end
    
  
let _ = main ()
