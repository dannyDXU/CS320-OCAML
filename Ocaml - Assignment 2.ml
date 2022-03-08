(************ PROBLEM safe dot-product ************)

(***
   Please, write a function that compute the dot product of two lists in a safe way.
   This function should produce a output no matter what input lists it is provided with.

   Note: dot product only exists for two vector of the same length:
   https://en.wikipedia.org/wiki/Dot_product#Algebraic_definition

   Example
   >>>> safe_dot_product [1;2;3] [2;2;0] = Some 6
   >>>> safe_dot_product [1;2] [2;3] = Some 8
*)

Property of Danny 

let rec r_dotProduct (x: int list) (y: int list): int  = 
    match x with
    |[] -> 0
    |h1::t1 -> (
       match y with
         |[]-> 0
         |h2::t2 ->
          (h1*h2) + r_dotProduct (t1) (t2)
 )
    
 let rec safe_dot_product (vect1: int list) (vect2: int list) : int option = 
  if (List.length vect1 != List.length vect2)
  then None
  else Some(r_dotProduct(vect1) (vect2))
  





(***
   Please, write another version of safe dot product which is tail recursive.
*)

let rec safe_dot_product_tail_rec (vect1: int list) (vect2: int list): int option = 
  let rec aux (x: int list) (y: int list) (z: int): int option =
     match (x, y) with
     |x::[], y::[] -> Some(x*y + z)
     | [], [] -> Some 0
     | x::_ , [] -> None
     | [], y::_ -> None
     | (x :: xs, y :: ys) -> aux (xs) (ys) (x*y + z)
    
       
       
    in aux vect1 vect2 0





(************ PROBLEM Bunny's Fibonacci ************)


(***
   Please, write a function that given a positive integer i returns the  number
   in position i in the bunny's Fibonacci Sequence defined as follows.

   The Bunny's Fibonacci Sequence looks like this [0;1;2;3;2;4;4;7;6;11;10...]
   where
   - the start of the sequence is 0;1;2;3
   - a number on an even index i is the sum of the numbers on the previous even
     indices, i.e. the numbers in positions (i-2) and (i-4).
     For example `2` on index 4 is a sum of `0` on index 0 and `2` on index 2
   - a number on an odd index j is the sum of the numbers on the previous odd
     indices   i.e. the numbers in positions (j-2) and (j-4).
     For example, `7` on index 7 is a sum of `3` on index 3 and `4` on index 5

   Example:
   (*The first element (index 0) of bunny's Fibonacci Sequence*)
   >>>> bunny_fib_idx 0 = 0
   (*The 5th element of (index 4) of bunny's Fibonacci Sequence*)
   >>>> bunny_fib_idx 4 = 2
   (*The 9th element of (index 8) of bunny's Fibonacci Sequence*)
   >>>> bunny_fib_idx 8 = 6
*)
let rec bunny_fib_idx (idx: int) (*Assume idx is positive*): int = 
  match idx with
  |0->0
  |1->1
  |2->2
  |3->3
  |idx -> (bunny_fib_idx(idx-2)) + (bunny_fib_idx (idx-4))


(***
   Implement a tail recursive version of bunny_fib_idx
   Your function should run in linear time.
*)




let bunny_fib_idx_tail_rec (idx: int) (*Assume idx is positive*): int =
  
  let rec helper (i:int) (one:int) (two:int) (three:int) (four: int): int =
  match idx with 
  |0 ->one
  |1->two
  |2->three
  |3->four
  |x-> 
  if i != idx then helper (i+1) (two) (three) (four) (one+three)
  else four
  in
  helper 3 0 1 2 3 
  

(************ PROBLEM Binary Addition ************)

(**
   Please, write a function to convert a list of integers 0 and 1 into boolean
   bits (true for 1, and false for 0).

   Your code needs to check whether there is any illegal number (non-0 and 1) in the list
*)


let rec illegal (lis: int list): bool  =
  match lis with
  |[]->false
  |x::[] -> if (x == 1 || x == 0) then false else true
  |x::xs ->  if (x == 1 || x== 0) then illegal xs else true

  
let rec int_list (lis: int list): bool list  =
    match lis with
    |[]->[]
    |x::xs ->   match x with
                 |1 -> true::(int_list xs)
                 |0 -> false::(int_list xs)     

let rec int_list_to_bits (int_lst: int list): bool list option = 
  if illegal(int_lst) == true then None else Some(int_list(int_lst))

                (***
   Please, write a function that takes in input two bits (booleans) and returns a pair where 
   - The first element is a boolean asserting wether there is a bit carrying over.
   - The second element is the resulting bit.
 ***)
let sum_bit (bit1: bool) (bit2: bool): (bool * bool) =
  match (bit1, bit2) with
  |(false, false) -> (false, false)
  |(false, true) -> (false, true)
  |(true, true) -> (true, false)
  |(true, false) -> (false, true)


(***
   Please, write a function that returns the sum of two list of bits (bool). 

   PRECONDITION:
   When adding bits1 and bits2 you can assume that the least significant bit is at the 
   head of the list and the most signifcant bit is at the end of the list. 

   --So for example the number 18 in binary starting from the most signifcant bit to the 
   least significant bit is 10010. 

   However the binary representation of 18 when passed into the sum_bits method  is 
   represented as 0 1 0 0 1 (i.e., starting from the least significant bit to the 
   most significant bit) which is equivalent to [false;true;false;false;true]

   --the number 8 in binary starting from the most signifcant bit to the 
   least significant bit is 1000. 

   However the binary representation of 8 when passed into the sum_bits method is 
   represented as 0 0 0 1 (i.e., starting from the least significant bit to the 
   most significant bit) which is equivalent to [false;false;false;true]

   POSTCONDITION:
   Now the result (18+8=26) of adding these two lists of bools is [false;true;false;true;true]
   i.e., in the output you also again put the least significant bit at the head of the list 
   and the most significant bit at the end of the list. 

   Here are some more examples:
   Example:
   (*1 + 1 = 10*)
   >>>> sum_bits [true] [true] = [false; true]
   (*11 + 1110 = 10001*)
   >>>> sum_bits [true; true] [false; true; true; true] = [true; false; false; false; true]
   (*0 + 110 = 110*)
   >>>> sum_bits [false] [false; true; true] = [false; true; true]

*)
let reverse list =
  let rec aux acc = function
    | [] -> acc
    |x::xs -> aux (x::acc) xs in aux [] list 

let rec sum_bits (bits1: bool list) (bits2: bool list): bool list = 
  let rec aux (x: bool list) (y: bool list) (z: int) (k: bool list): bool list =
    match (x, y, z, k) with
    |([], [], 0, k) -> reverse(k)
    |([], [], 1, k) -> aux ([]) ([]) (0) ((true)::k)
    |([], y::ys, 0, k) -> (match (y) with
                          |true -> aux ([]) (ys) (0) ((true)::k)
                          |false -> aux ([]) (ys) (0) ((false)::k)
    )
    |([], y::ys, 1, k) -> (match (y) with
                          |false -> aux ([]) (ys) (0) ((true)::k)
                          |true -> aux ([]) (ys) (1) ((false)::k)
    )
    |(x::xs, [], 0, k) -> (match (x) with
                          |true -> aux (xs) ([]) (0) ((true)::k)
                          |false -> aux (xs) ([]) (0) ((false)::k)
    )
    |(x::xs, [], 1, k) -> (match (x) with
                          |false -> aux (xs) ([]) (0) ((true)::k)
                          |true -> aux (xs) ([]) (1) ((false)::k)
    )
    |(x::[], y::[], 0, k) -> (match (x,y) with
                      |(false, false) -> aux ([]) ([]) (0) ((false)::k)
                      |(false, true) ->  aux ([]) ([]) (0) ((true)::k)
                      |(true, true) ->   aux ([]) ([]) (1) ((false)::k)
                      |(true, false) ->  aux ([]) ([]) (0) ((true)::k)
    )
    |(x::[], y::[], 1, k) -> (match (x,y) with
                      |(false, false) -> aux ([]) ([]) (0) ((true)::k)
                      |(false, true) ->  aux ([]) ([]) (1) ((false)::k)
                      |(true, true) ->   aux ([]) ([]) (1) ((true)::k)
                      |(true, false) ->  aux ([]) ([]) (1) ((false)::k)
    )
    |(x::xs, y::ys, 0, k) -> (match (x, y) with 
                      |(false, false) -> aux (xs) (ys) (0) ((false)::k)
                      |(false, true) ->  aux (xs) (ys) (0) ((true)::k)
                      |(true, true) ->   aux (xs) (ys) (1) ((false)::k)
                      |(true, false) ->  aux (xs) (ys) (0) ((true)::k)
    )
    |(x::xs, y::ys, 1, k) -> (match (x,y) with
                      |(false, false) -> aux (xs) (ys) (0) ((true)::k)
                      |(false, true) ->  aux (xs) (ys) (1) ((false)::k)
                      |(true, true) ->   aux (xs) (ys) (1) ((true)::k)
                      |(true, false) ->  aux (xs) (ys) (1) ((false)::k)
    )
    
         in aux (bits1) (bits2) (0) ([])




  
(***
   Please, write a function that converts a list of bits back to a list of 0s and 1s
   (true for 1, and false for 0).
*)
let rec bits_to_int_list (bits: bool list): int list = 
  match bits with
  |[]->[]
  |x::xs ->   match x with
               |true -> 1::(bits_to_int_list xs)
               |false-> 0::(bits_to_int_list xs) 


(***
   Please write a function that sums two binary number, 
   where each number is represented by a list of 0s and 1s.
   For example the binary number 10010 will be represented by the list [1;0;0;1;0]

   Your function needs to deal carefully with invalid inputs like the following:
   - the input list is empty
   - the input list contains elements that are not 0 or 1
   - the input list is a non-singleton list starting with 0 (like [0,1,1,1])

   Hint: use the functions that you defined previously, in this exercise, as helper functions. 

   Example:
   >>>> sum_bin [1] [1] = Some [1;0]
   >>>> sum_bin [1;1] [1;1;1;0] = Some [1;0;0;0;1]
   >>>> sum_bin [0] [1;1;0] = Some [1;1;0]
 ***)
   
let rec sum_binss (b_num1: int list) (b_num2: int list): int list  =
    let rec aux (x: int list) (y: int list) (z: int) (k: int list): int list =
      match (x, y, z, k) with
      |([], [], 0, k) -> (k)
      |([], [], 1, k) -> aux ([]) ([]) (0) ((1)::k)
      |([], y::ys, 0, k) -> (match (y) with
                            |1 -> aux ([]) (ys) (0) ((1)::k)
                            |0 -> aux ([]) (ys) (0) ((0)::k)
      )
      |([], y::ys, 1, k) -> (match (y) with
                            |0 -> aux ([]) (ys) (0) ((1)::k)
                            |1 -> aux ([]) (ys) (1) ((0)::k)
      )
      |(x::xs, [], 0, k) -> (match (x) with
                            |1 -> aux (xs) ([]) (0) ((1)::k)
                            |0 -> aux (xs) ([]) (0) ((0)::k)
      )
      |(x::xs, [], 1, k) -> (match (x) with
                            |0 -> aux (xs) ([]) (0) ((1)::k)
                            |1 -> aux (xs) ([]) (1) ((0)::k)
      )
      |(x::[], y::[], 0, k) -> (match (x,y) with
                        |(0, 0) -> aux ([]) ([]) (0) ((0)::k)
                        |(0, 1) ->  aux ([]) ([]) (0) ((1)::k)
                        |(1, 1) ->   aux ([]) ([]) (1) ((0)::k)
                        |(1, 0) ->  aux ([]) ([]) (0) ((1)::k)
      )
      |(x::[], y::[], 1, k) -> (match (x,y) with
                        |(0, 0) -> aux ([]) ([]) (0) ((1)::k)
                        |(0, 1) ->  aux ([]) ([]) (1) ((0)::k)
                        |(1, 1) ->   aux ([]) ([]) (1) ((1)::k)
                        |(1, 0) ->  aux ([]) ([]) (1) ((0)::k)
      )
      |(x::xs, y::ys, 0, k) -> (match (x, y) with 
                        |(0, 0) -> aux (xs) (ys) (0) ((0)::k)
                        |(0, 1) ->  aux (xs) (ys) (0) ((1)::k)
                        |(1, 1) ->   aux (xs) (ys) (1) ((0)::k)
                        |(1, 0) ->  aux (xs) (ys) (0) ((1)::k)
      )
      |(x::xs, y::ys, 1, k) -> (match (x,y) with
                        |(0, 0) -> aux (xs) (ys) (0) ((1)::k)
                        |(0, 1) ->  aux (xs) (ys) (1) ((0)::k)
                        |(1, 1) ->   aux (xs) (ys) (1) ((1)::k)
                        |(1, 0) ->  aux (xs) (ys) (1) ((0)::k)
      )
  
      
           in aux (b_num1) (b_num2) (0) ([])


let illegal_zeros (num: int list): bool =
  match num with
  |[]->false
  |0::[]->false
  |0::xs->true
  |num -> false
  


let rec sum_bin (b_num1: int list) (b_num2: int list): int list option =
  if (illegal(b_num1 @ b_num2) == true) || ((b_num1 == []) || (b_num2 == [])) || illegal_zeros(b_num1) == true || illegal_zeros(b_num2) == true  then None else Some(sum_binss(reverse(b_num1))(reverse(b_num2)))