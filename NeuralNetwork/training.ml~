class training () = 
object (self)

(* renvoie les ouputs du reseau de neurones pour chaque cas donné par les inputs *)
method get_result inputs (anna:ann) =
  let result = Array.make_matrix (Array.length inputs) 1 1. in
  for i = 0 to (Array.length inputs)-1 do
    (anna#forward_propagation inputs.(i) (anna#get 1) (anna#get 2));
    result.(i)<- anna#get_output 1;
  done;
    result

 (* fait la transposée d'une matrice *)
method t (mat:float array array) =
  let width = Array.length mat.(0) 
  and heigth = Array.length mat in
  let sol = Array.make_matrix width heigth mat.(0).(0) in
  for i = 0 to (Array.length mat)-1 do
    for j = 0 to (Array.length mat.(0))-1 do
      sol.(j).(i) <- mat.(i).(j)
    done;
  done;
  sol

(* fait la multiplacation d'une matrice et d'un vecteur *)
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
(* Calcul de l'erreur d'une hidden layer *)
method delta mw error (activation:float array) = 
  let result = self#mult (self#t mw) error
  and sol = Array.make (Array.length activation) 1. in
  for i = 0 to Array.length sol-1 do
    sol.(i)<- result.(i+1) *. (activation.(i) *. (1. -. activation.(i)))
  done;
  sol

(* fait une multiplacation de deux vecteur X,1 pour avoir une matrice X,X *)
method mat_mult ar1 ar2 = 
  let sol = Array.make_matrix (Array.length ar1) (Array.length ar2) 1. 
  in
  for i = 0 to Array.length ar1 - 1 do
    for j = 0 to Array.length ar2 - 1 do
      sol.(i).(j) <- ar1.(i) *. ar2.(j)
    done;
  done;
  sol

(* Calcule l'erreur de l'ouput layer *)
method first_error activation hopes  = 
  let result = Array.make (Array.length hopes) 1. in
  for i = 0 to Array.length result -1 do
    result.(i) <- (activation.(i) -. hopes.(i));
  done;
  result

(* met à jour les poids du NN *)
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

(* somme membre à membre de deux vecteurs *)  
method sum ar1 ar2 = 
  let sol = Array.make (Array.length ar1) 1. in
  for i = 0 to Array.length sol -1 do
    sol.(i) <- ar1.(i) +. ar2.(i)
  done;
  sol
(* Somme membre à membre de deux matrics *)
method sum_mat m1 m2 = 
  let sol = Array.make_matrix (Array.length m1) (Array.length m1.(0)) 0.
  in
  for i = 0 to Array.length sol -1 do
    for j = 0 to Array.length sol.(0) -1 do
      sol.(i).(j) <- m1.(i).(j) +. m2.(j).(i)
    done;
  done;
  sol
(* Multiplication membre à membre de deux vecteurs *)
method test ar1 ar2 = 
  let sol = Array.make (Array.length ar2) 1. in
  for i = 0 to Array.length sol -1 do
    sol.(i) <- ar1.(i) *. ar2.(i);
  done;
  sol


(* Divise l'erreur des couches internes par le nombre de cas d'entrainement *)
method div_delta m m1 = 
  let sol = Array.make_matrix (Array.length m1) (Array.length m1.(0)) 0.
in
  for i = 0 to Array.length m1 -1 do
    for j = 0 to Array.length m1.(0) -1 do
      sol.(i).(j) <- m1.(i).(j) /. (float_of_int) m
    done;
  done;
  sol

(* self explanatory *)
method train (p:ann) inputs outputs =
  let errors = Array.make_matrix 3 2 1.
  and delta1 =ref  (Array.make_matrix (Array.length (p#get 1)) (Array.length (p#get 1).(0)-1) 0.)
  and delta2 =ref  (Array.make_matrix (Array.length (p#get 2)) (Array.length (p#get 2).(0)-1) 0.)
  in
  errors.(0) <- Array.make 1 0.;
  errors.(1) <- Array.make 2 0.;
  errors.(2) <- Array.make 2 0.;
  for i = 0 to 100000 do
    for z = 0 to Array.length inputs -1 do
      p#forward_propagation inputs.(z) (p#get 1) (p#get 2);
      errors.(0) <- self#first_error (p#get_output 1) (outputs.(z));
      errors.(1) <- (self#delta (p#get 2) errors.(0) (p#get_output 0)) ;
      errors.(2) <- (self#delta (p#get 1) errors.(1) inputs.(z));
      delta1 := self#sum_mat (!delta1) (self#mat_mult (inputs.(z)) errors.(1));
      delta2 := self#sum_mat (!delta2) (self#mat_mult (p#get_output 0) errors.(0));
    done;
    delta1 := self#div_delta (Array.length inputs) !delta1;
    delta2 := self#div_delta (Array.length inputs) !delta2;
    self#update_weights (p#get 1) (p#get 2) !delta1 !delta2;
    delta1 := (Array.make_matrix (Array.length (p#get 1)) (Array.length (p#get 1).(0)-1) 0.);
    delta2 := (Array.make_matrix (Array.length (p#get 2)) (Array.length (p#get 2).(0)-1) 0.);
    errors.(0) <- Array.make 1 0.;
    errors.(1) <- Array.make 2 0.;
    errors.(2) <- Array.make 2 0.;
  done;
  for q = 0 to Array.length inputs - 1 do
    p#forward_propagation  inputs.(q) (p#get 1) (p#get 2);
    p#get_result;
  done;

  

end;;
