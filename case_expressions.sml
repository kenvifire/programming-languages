datatype mytype = TwoInts of int * int
              | Str of string
              | Pizza

(* mytype -> int*)

fun f (x : mytype) =
    case x of
         Pizza => 3
        | Str s => 8
        | TwoInts(i1,i2) => i1+i2

fun nondecreasing xs =
	case xs of
		[] => true
	  | _::[] => true
      | head::(neck::rest) => head <= neck
                              andalso nondesreasing (neck::rest)



