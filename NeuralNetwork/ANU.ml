class anu =
object
(* simulates a neuron, basically just a float, shouldn't have done that *)
  val mutable activation = 0.
  method chg a = activation <- a
  method get = activation
end;;
