class training ann = 
object (self)
val mutable anna  = new ann

method get_result inputs (anna:ann) =
  let result = Array.make_matrix (Array.length inputs) 1 1. in
  for i = 0 to (Array.length inputs)-1 do
    (anna#forward_propagation inputs.(i) (anna#get 1) (anna#get 2));
    result.(i)<- anna#get_output 1;
  done;
    result
 
method t (mat:float array array) = 
  let sol = Array.make_matrix (Array.length mat.(0)) (Array.length mat) mat.(0).(0) in
  for i = 0 to (Array.length mat)-1 do
    for j = 0 to (Array.length mat.(0))-1 do
      sol.(j).(i) <- mat.(i).(j)
    done;
  done;
  sol


method mult mat1 mat2 =
  let result = Array.make (Array.length mat1) 1. in
  for i = 0 to (Array.length mat1)-1 do
    let sol = ref 0. in
    for z = 0 to (Array.length mat2)-1 do
      sol := !sol +. mat1.(i).(z)*.mat2.(z) 
    done;
    result.(i) <- !sol;
    sol := 0.;
  done;
  result

method delta mw error (activation:float array) = 
  let result = self#mult (self#t mw) error
  and sol = Array.make (Array.length activation) 1. in
  for i = 0 to Array.length sol-1 do
    sol.(i)<- result.(i+1) *. (activation.(i) *. (1. -. activation.(i)))
  done;
  sol

method mat_mult ar1 ar2 = 
  let sol = Array.make_matrix (Array.length ar1) (Array.length ar2) 1. 
  in
  for i = 0 to Array.length ar1 - 1 do
    for j = 0 to Array.length ar2 - 1 do
      sol.(i).(j) <- ar1.(i) *. ar2.(j)
    done;
  done;
  sol

method first_error activation hopes  = 
  let result = Array.make (Array.length hopes) 1. in
  for i = 0 to Array.length result -1 do
    result.(i) <- (activation.(i) -. hopes.(i));
  done;
  result

method update_weights mw1 mw2 d1 d2  = 
  for i = 0 to Array.length d1 -1 do
    for j = 0 to Array.length d1.(0)-1 do
      mw1.(i).(j+1) <- (mw1.(i).(j+1) -. (d1.(i).(j))*.0.2);
    done;
  done;
  for i = 0 to Array.length d2 -1 do
      for j = 0 to Array.length d2.(0) -1 do
      mw2.(i).(j+1) <- mw2.(i).(j+1) -. (d2.(i).(j)*.0.2);
    done;
  done;
  (mw1,mw2)
  
method sum ar1 ar2 = 
  let sol = Array.make (Array.length ar1) 1. in
  for i = 0 to Array.length sol -1 do
    sol.(i) <- ar1.(i) +. ar2.(i)
  done;
  sol

method sum_mat m1 m2 = 
  let sol = Array.make_matrix (Array.length m1) (Array.length m1.(0)) 0.
  in
  for i = 0 to Array.length sol -1 do
    for j = 0 to Array.length sol.(0) -1 do
      sol.(i).(j) <- m1.(i).(j) +. m2.(j).(i)
    done;
  done;
  sol

method test ar1 ar2 = 
  let sol = Array.make (Array.length ar2) 1. in
  for i = 0 to Array.length sol -1 do
    sol.(i) <- ar1.(i) *. ar2.(i);
  done;
  sol

method div_delta m m1 = 
  let sol = Array.make_matrix (Array.length m1) (Array.length m1.(0)) 0.
in
  for i = 0 to Array.length m1 -1 do
    for j = 0 to Array.length m1.(0) -1 do
      sol.(i).(j) <- m1.(i).(j) /. (float_of_int) m
    done;
  done;
  sol
method train (p:ann) inputs outputs =
  let prout = Array.make_matrix 3 2 1.
  and caca = Array.make 3 1.
  and delta1 =ref  (Array.make_matrix (Array.length (p#get 1)) (Array.length (p#get 1).(0)-1) 0.)
  and delta2 =ref  (Array.make_matrix (Array.length (p#get 2)) (Array.length (p#get 2).(0)-1) 0.)
  in
  prout.(0) <- Array.make 1 0.;
  prout.(1) <- Array.make 2 0.;
  prout.(2) <- Array.make 2 0.;
  for i = 0 to 100000 do
    for z = 0 to Array.length inputs -1 do
      p#forward_propagation inputs.(z) (p#get 1) (p#get 2);
      (* je fait la somme trop tot, il faut calcucler les derivatives avant *)
      prout.(0) <- self#first_error (p#get_output 1) (outputs.(z));
      prout.(1) <- (self#delta (p#get 2) prout.(0) (p#get_output 0)) ;
      prout.(2) <- (self#delta (p#get 1) prout.(1) inputs.(z));
      delta1 := self#sum_mat (!delta1) (self#mat_mult (inputs.(z)) prout.(1));
      delta2 := self#sum_mat (!delta2) (self#mat_mult (p#get_output 0) prout.(0));
      (*for lo = 0 to (Array.length prout.(0))-1 do
	print_float prout.(0).(lo);
	print_newline();
      done;*)
    done;
    delta1 := self#div_delta (Array.length inputs) !delta1;
    delta2 := self#div_delta (Array.length inputs) !delta2;
    self#update_weights (p#get 1) (p#get 2) !delta1 !delta2;
    delta1 := (Array.make_matrix (Array.length (p#get 1)) (Array.length (p#get 1).(0)-1) 0.);
    delta2 := (Array.make_matrix (Array.length (p#get 2)) (Array.length (p#get 2).(0)-1) 0.);
    prout.(0) <- Array.make 1 0.;
    prout.(1) <- Array.make 2 0.;
    prout.(2) <- Array.make 2 0.;
  done;
  for q = 0 to Array.length inputs - 1 do
    p#forward_propagation  inputs.(q) (p#get 1) (p#get 2);
    p#get_result;
  done;

  

end;;
let fonc (p:ann) (j:training) inputs outputs =
  let prout = Array.make_matrix 3 2 1.
  and caca = Array.make 3 1.
  and delta1 =ref  (Array.make_matrix (Array.length (p#get 1)) (Array.length (p#get 1).(0)-1) 0.)
  and delta2 =ref  (Array.make_matrix (Array.length (p#get 2)) (Array.length (p#get 2).(0)-1) 0.)
  in
  prout.(0) <- Array.make 1 0.;
  prout.(1) <- Array.make 2 0.;
  prout.(2) <- Array.make 2 0.;
  for i = 0 to 100000 do
    for z = 0 to Array.length inputs -1 do
      p#forward_propagation inputs.(z) (p#get 1) (p#get 2);
      (* je fait la somme trop tot, il faut calcucler les derivatives avant *)
      prout.(0) <- self#first_error (p#get_output 1) (outputs.(z));
      prout.(1) <- (self#delta (p#get 2) prout.(0) (p#get_output 0)) ;
      prout.(2) <- (self#delta (p#get 1) prout.(1) inputs.(z));
      delta1 := self#sum_mat (!delta1) (self#mat_mult (inputs.(z)) prout.(1));
      delta2 := self#sum_mat (!delta2) (self#mat_mult (p#get_output 0) prout.(0));
      (*for lo = 0 to (Array.length prout.(0))-1 do
	print_float prout.(0).(lo);
	print_newline();
      done;*)
    done;
    delta1 := self#div_delta (Array.length inputs) !delta1;
    delta2 := self#div_delta (Array.length inputs) !delta2;
    j#update_weights (p#get 1) (p#get 2) !delta1 !delta2;
    delta1 := (Array.make_matrix (Array.length (p#get 1)) (Array.length (p#get 1).(0)-1) 0.);
    delta2 := (Array.make_matrix (Array.length (p#get 2)) (Array.length (p#get 2).(0)-1) 0.);
    prout.(0) <- Array.make 1 0.;
    prout.(1) <- Array.make 2 0.;
    prout.(2) <- Array.make 2 0.;
  done;
  for q = 0 to Array.length inputs - 1 do
    p#forward_propagation  inputs.(q) (p#get 1) (p#get 2);
    p#get_result;
  done;;
  
let p = new ann 2 2 1;;
let j = new training 1 ;;
let inputs = Array.make_matrix 4 2 1.;;
p#get 1;;
p#get 2;;
inputs.(1).(1) <- 0.;;
inputs.(2).(0) <- 0.;;
inputs.(3).(0) <- 0.;;
inputs.(3).(1) <- 0.;;
let outputs = Array.make_matrix 4 1 1.;;
outputs.(1).(0) <- 0.;;
outputs.(2).(0) <- 0.;;
outputs;;
p#forward_propagation inputs.(1) (p#get 1) (p#get 2);;
p#get_output 1;;
p#get_output 0;;
fonc p j inputs outputs;;
let test  = Array.make 2 1.;;
p#forward_propagation test (p#get 1) (p#get 2);;
p#get_result;;
let results = p#get_output 1;;
let hopes = Array.make 1 1.;;
let error1=j#first_error results hopes;;
let error2 = j#delta (p#get 2) error1 (p#get_output 0);;
let error3 = j#delta (p#get 1) error2 inputs.(2);;
let prout = Array.make_matrix 3 1 1.;; 
let caca = j#t (p#get 2);;
let test = j#mult caca error1;;
j#mult (j#t (p#get 1)) error2;;
inputs.(0);;
prout.(0) <- error1;;
prout.(1) <- error2;;
prout.(2) <- error3;;
j#update_weights (p#get 1) (p#get 2) prout;;
p#forward_propagation inputs.(0) (p#get 1) (p#get 2);;
p#get_result;;
prout.(2);;
prout;;
p#get 1;;
p#get 2;;
p#get_output 0;;
p#get_output 1;;
error1;;
error2;;
Array.length (p#get 0).(0) -1 ;;
Array.length prout.(0) -1;;
caca;;
error1;;
(p#get 2).(0);;
