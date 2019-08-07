

(*----------------------------------------------------------------------------------
exception Invalid_Input_exception of string
Used for raising exception for invalid input to factorial function
------------------------------------------------------------------------------------*)
exception Invalid_Input_exception of string


(*----------------------------------------------------------------------------------
val validateInput = fn : string -> bool
check if input is valid or not
------------------------------------------------------------------------------------*)
fun validateInput(s: string) = 
	if s = "" then false
	(*
		check for each character in string if it is a digit or not
	*)
	else 
		List.all (Char.isDigit) (explode s)




(*----------------------------------------------------------------------------------
val fromStringHelper = fn : string -> int list 
Converts input string to int list where each element of list is in 10^4 base
------------------------------------------------------------------------------------*)
fun fromStringHelper(s: string) = 
	if s = "" then []
	else
		if String.size(s) >= 4 then
			let
				(*
					msb_size is length of 1st digit in 10^4 base if size of string is not multiple of 4. 
					Split string s in s0 and s1 where s0 is msb digit and s1 remaining in s
				*)
			 	val msb_size = String.size(s) mod 4;
			 	val s0 = String.extract(s,0, SOME msb_size);
			 	val s1 = String.extract(s, msb_size, NONE);
			in
			 	(*
			 		if size of s is multiple of 4 then split 1st 4 from string and cons to remaining list
			 	*)
			 	if msb_size = 0 then
			 		valOf(Int.fromString(String.extract(s,0, SOME 4))) :: fromStringHelper(String.extract(s,4, NONE))
			 	(*
			 		if size of s is not multiple of 4 then cons msb digit to remaining list
			 	*)
			 	else 
			 		valOf(Int.fromString(s0)) :: fromStringHelper(s1)	
			end 
		(*
			if length of s is less than 4 then simply make it list
		*)
		else
			valOf(Int.fromString(s))::[];


(*----------------------------------------------------------------------------------
val fromString = fn : string -> int list 
Converts input string to int list where each element of list is in 10^4 base 
by taking help of fromStringHelper function
------------------------------------------------------------------------------------*)
fun fromString(s: string) = 
	(*
		if input is valid then covert to int list
		else raise exception
	*)
	if validateInput s then 
		fromStringHelper(s)
	else
		raise Invalid_Input_exception "Invalid input"


(*----------------------------------------------------------------------------------
val to4digits = fn : int -> string
Converts any digit in base 10^4 to 4 length string by appending 0 in front
------------------------------------------------------------------------------------*)
fun to4digits(n: int) =
	let
		(*
			n_string is string representation of n
			l is length of n_string
		*)
		val n_string = Int.toString(n);
		val l = String.size(n_string);
	in
		(*
			Apeend 0's in front to make it 4 digits
		*)
		if l = 4 then n_string
		else if l = 3 then "0" ^ n_string	
		else if l = 2 then "00" ^ n_string	
		else  "000" ^ n_string			
	end



(*----------------------------------------------------------------------------------
val trimList = fn : int list -> int list
Trims the leading 0's in the list
------------------------------------------------------------------------------------*)
fun trimList(l: int list) = 
	if length l > 1 then
		if hd l = 0 then trimList(tl l)
		else l
	else
		l;

(*----------------------------------------------------------------------------------
val trimString = fn : string -> string
Trims the leading 0's in the string
------------------------------------------------------------------------------------*)
fun trimString(s: string) = 
	if s = "" then s
	else if substring(s, 0, 1) = "0" then 
		trimString(String.extract(s, 1, NONE))
	else 
		s;


(*----------------------------------------------------------------------------------
val toStringHelper = fn : int list -> string
Converts int list to corresponding string
------------------------------------------------------------------------------------*)
fun toStringHelper(l: int list) =
	 (*
		if list is null then empty string
	*)
	if null l then ""
	(*
		concate 4 digit representation of head with converted tail
	*)
	else
		let
			val head = to4digits(hd l);
			val tail = tl l;
		in
			head ^ toStringHelper(tail)
		end



(*----------------------------------------------------------------------------------
val toString = fn : int list -> string
Converts int list to corresponding string
------------------------------------------------------------------------------------*)
fun toString(l: int list) = 
	(*
		if hd of list is 4 digits long then directly convert to string
	*)
	if hd l > 999 then toStringHelper(l)
	(*
		if hd list is less than 4 digits then convert head to string separately 
		and concate result with string representation of tail
	*)
	else
		let
			val head = Int.toString(hd l);
			val tail = tl l;
		in
			head ^ toStringHelper(tail)
		end






(*----------------------------------------------------------------------------------
val countDigits = fn : int list -> int
count the no. of digits in integer list in base 10^4, pass start value 0
------------------------------------------------------------------------------------*)
fun countDigits(x: int list) =
	length x


(*----------------------------------------------------------------------------------
val minimum = fn : int * int -> int
return minimum of two no
------------------------------------------------------------------------------------*)
fun minimum(n1: int, n2: int) = 
	if n1 < n2 then n1 else n2


(*----------------------------------------------------------------------------------
val splitList = fn : int list * int -> int list * int list
Split the list in two given postion of splitting point
pos is counted from right side of list
------------------------------------------------------------------------------------*)
fun splitList(n: int list, pos: int) =
	List.splitAt(n, length n - pos)


(*----------------------------------------------------------------------------------
val addSingleDigitInt = fn : int * int * int -> int * int
Add two single digit with carry in base 10^4 and return tuple with sum and carry
------------------------------------------------------------------------------------*)
fun addSingleDigitInt(n1: int, n2: int, cin: int) =
	let
		val s = n1 + n2 + cin;
		val sum = s mod 10000;
		val carry_out = s div 10000;
	in
		(sum, carry_out)
	end


(*----------------------------------------------------------------------------------
val multiplySingleDigitInt = fn : int * int -> int * int
Multiply two single digit in base 10^4 return tuple with result and carry
------------------------------------------------------------------------------------*)
fun multiplySingleDigitInt(n1: int, n2: int) =
	let
		val m = n1 * n2;
		val mult = m mod 10000;
		val carry_out = m div 10000;
	in
		(mult, carry_out)
	end


(*----------------------------------------------------------------------------------
val addList = fn : int list * int list * int * int list -> int list
Add the two list with carry in base 10^4. 
Call with carry_in = 0 and partial_sum=[]
------------------------------------------------------------------------------------*)
fun addList(l1: int list, l2: int list, carry_in: int, partial_sum: int list) = 
	(*
		if both list is null then return carry with partial_sum
	*)
	if null l1 andalso null l2 then 
		trimList(carry_in::partial_sum)

	(*
		if only one list is null then return other list
	*)
	else if null l1 then
		addList(0::[], l2, carry_in, partial_sum)
	else if null l2 then
		addList(l1, 0::[], carry_in, partial_sum)
	else
		let
			(*
				Last1 and Last2 is right digit in base 10^4 in both list to be added
				sum and carry is the result of sum with carry_in of last digit of l1 and l2
				sum will be appended in partial sum
				new_l1 and new_l2 is remaing list to be added
			*)
			val last1 = List.nth(l1, (length l1) - 1)
			val last2 = List.nth(l2, (length l2) - 1)
			val s = addSingleDigitInt(last1, last2, carry_in)
			val sum = #1 s;
			val carry_out = #2 s;

			val new_l1 = List.take(l1, length l1- 1)
			val new_l2 = List.take(l2, length l2 - 1)
		in
			addList(new_l1, new_l2, carry_out, sum::partial_sum )
	 
		end



(*----------------------------------------------------------------------------------
val greaterThanList = fn : int list * int list -> bool
Compare 1st list and 2nd list.
Output true is 1st list is greater than or equal to 2nd list
------------------------------------------------------------------------------------*)
fun greaterThanList(l1: int list, l2: int list) = 
	let
		val l1 = trimList(l1);
		val l2 = trimList(l2)
	in
		(*
			if l2's length is bigger then l2 is bigger
			if l1's length is bigger then l1 is bigger
		*)
		if length l1 < length l2 then false
		else if length l1 > length l2 then true
		(*
			if both length equal then compare msb of both
		*)
		else 
			if null l1 then true
			else if hd l1 < hd l2 then false
			else if hd l1 > hd l2 then true
			(*
				if both length equal and msb are equal then compare next msb of both
			*)
			else
				greaterThanList(tl l1, tl l2)	
	end
		

(*----------------------------------------------------------------------------------
val complementList = fn : int list -> int list
Output 9999's (r-1)'s complement of list like 1's complement in binary
------------------------------------------------------------------------------------*)
fun complementList(l: int list) = 
	if null l then []
	else 
		let
			val head = hd l;
			val tail = tl l;

			val complemnted_head = 9999 - head
		in
			complemnted_head::complementList(tail)
		end


(*----------------------------------------------------------------------------------
val padComplement = fn : int list * int -> int list
pad complemented no with 9999 in left side. It is used for subtraction
------------------------------------------------------------------------------------*)
fun padComplement(l: int list, count: int) = 
	if count = 0 then l
	else 
		padComplement(9999::l, count - 1 )


(*----------------------------------------------------------------------------------
val subtractList = fn : int list * int list -> int list
Subtract two list by r's complement like 2's complement subtraction in binary. 
It is subtracting smaller no. from bigger
a - b = a + ~b + 1
------------------------------------------------------------------------------------*)
fun subtractList(l1: int list, l2: int list) = 
	let
		val l1 = trimList(l1);
		val l2 = trimList(l2);
	in
		(*
			if any one list is nil then result 2nd list
		*)
		if null l1 then l2 
		else if null l2 then l1

		(*
			if list l1 >= list l2 then do subtraction l1 - l2
			l1 - l2 = l1 + ~l2 + 1
			It will result carry, we ignore carry
		*)
		else if greaterThanList(l1, l2) then 
			let
				val complemented_l2 = complementList(l2);
				val pad_complemented_l2 = padComplement(complemented_l2, length l1 - length l2)
				val add = addList(l1 , pad_complemented_l2, 0, []);
				val sub = addList(add , [1], 0, []);
			in
				tl sub
			end
		(*
			if list l2 >= list l1 then do subtraction l2 - l2
		*)
		else 
			subtractList(l2, l1)	
	end
		

(*----------------------------------------------------------------------------------
val calcZ1 = fn : int list * int list * int list * int list * int list * int list * int list -> int list
It calculates z1 value used in karatsuba algorithm
z1 = sgn(x0 - x1):sgn(y1 - y0).|x0 - x1|.|y1 - y0| + x1y1 + x0y0
we already pass the |x0 - x1|.|y1 - y0| value in z1_t
------------------------------------------------------------------------------------*)
fun calcZ1(z0: int list, z1_t: int list, z2: int list, x0: int list, x1: int list, y0: int list, y1: int list) =
	let
		(*
			Find the sign of (x0 - x1) and (y1 - y0)
			then find the sign of (x0 - x1).(y1 - y0) using xnor
		*)
		val sign1 = greaterThanList(x0, x1)
		val sign2 = greaterThanList(y1, y0)
		val sign = (sign1 andalso sign2) orelse (not(sign2) andalso not(sign1)) (*xnor*)	 
	in
		if sign then 
			(* 
				z1_t + z0 + z2
			*)
			addList( addList(z1_t, z0, 0, []), z2, 0, [] )
		else
			(*
				-z1_t + z0 + z2
			*)
			subtractList( addList(z0, z2, 0, []), z1_t)
			
	end

(*----------------------------------------------------------------------------------
val shiftLeft = fn : int list * int -> int list
Left shif list count times and append 0 in the last
------------------------------------------------------------------------------------*)
fun shiftLeft(l: int list, count: int) = 
	if count = 0 then l
	else 
		shiftLeft(l @ [0], count - 1)	

(*----------------------------------------------------------------------------------
val padLeft = fn : int list * int -> int list
Pad left of list with 0 count times
------------------------------------------------------------------------------------*)
fun padLeft(l: int list, count: int) = 
	if count = 0 then l
	else 
		padLeft([0] @ l, count - 1)	


(*----------------------------------------------------------------------------------
val makeEqualLengthList = fn : int list * int list -> int list * int list
It takes two list and pad left smaller list to make its size equal to large list
It return two equal length list in tuple
------------------------------------------------------------------------------------*)
fun makeEqualLengthList(l1: int list, l2: int list) = 
	if length l1 = length l2 then 
		(l1, l2)
	else if length l1 > length l2 then
		(l1, padLeft(l2, (length l1) - (length l2)))
	else
		(padLeft(l1, (length l2) - (length l1)), l2)


(*----------------------------------------------------------------------------------
val karatsuba = fn : int list -> int list -> int list
Karatsuba for Multiplying two int list
------------------------------------------------------------------------------------*)
fun karatsuba(n1: int list) = 
	fn(n2: int list) => 
		let
			(*
				make both list of equal lenght
			*)
			val tuple_list =  makeEqualLengthList(n1, n2);
			val n1 = #1 tuple_list;
			val n2 = #2 tuple_list;
		in
			(*
				if both list contains single digit then multiply directly
			*)
			if length n1 = 1 andalso length n2 = 1 then
				let
					val m = multiplySingleDigitInt( hd n1, hd n2)
					val mult = #1 m;
					val carry_out = #2 m 
				in
					if carry_out = 0 then [mult]
					else [carry_out, mult]
				end
			(*
				if any of list is null then output [0] directly
			*)
			else if null n1 orelse null n2 then [0] 
			else
				let
					(*
						Karatsuba Algorithm

						x = x1Bm + x0
						y = y1Bm + y0
						x.y = x1y1B2m + (x1y0 + x0y1)Bm + x0y0

						z2 = x1y1
						z1 = x1y0 + x0y1 =  sgn(x0 - x1).sgn(y1 - y0).|x0 - x1|.|y1 - y0| + x1y1 + x0y0
						z0 = x0y0

						xy = z2B2m + z1Bm + z0

					*)
					val mid = Real.ceil(Real.fromInt( minimum( countDigits(n1), countDigits(n2) ) ) / 2.0);

					val x = splitList(n1, mid);
					val y = splitList(n2, mid)

					val x1 = #1 x;
					val x0 = #2 x;

					val y1 = #1 y;
					val y0 = #2 y;

					(*val x1 = #1 (splitList(n1, mid));
					val x0 = #2 (splitList(n1, mid));

					val y1 = #1 (splitList(n2, mid));
					val y0 = #2 (splitList(n2, mid));*)

					val z0 = karatsuba x0 y0
					val z2 = karatsuba	x1 y1
					val z1_t = karatsuba (subtractList(x0, x1)) (subtractList(y1, y0))
					val z1 = calcZ1(z0, z1_t, z2, x0, x1, y0, y1)

					(*
						xy = z2B2m + z1Bm + z0

						t2: shift z2 2m times for z2B^2m
						t1: shift z1 m times for z1B^m
						t0: z0
						result will be addition of t2, t1 and to
					*)
					val t2 = shiftLeft(z2, 2*(mid))
					val t1 =  shiftLeft(z1, mid)
					val t0 =  z0

					val add1 = addList(t2, t1, 0, [])
					val add2 = addList(add1, t0, 0, [])

					val final = add2
				in
					add2
				end
		end

(*----------------------------------------------------------------------------------
val isLessThanEqualTo1 = fn : int list -> bool
check if list is [0] or [1]
------------------------------------------------------------------------------------*)
fun isLessThanEqualTo1(l: int list) =
	let
	 	val l = trimList(l)
	in
	 	if length l = 1 andalso (hd l = 1 orelse hd l = 0 ) then
	 		true
	 	else
	 		false
	 end 	



(*----------------------------------------------------------------------------------
val decreaseBy1 = fn : int list -> int list
Subtract [1] from list l
------------------------------------------------------------------------------------*)	 	
fun decreaseBy1(l: int list) = 
	subtractList(l, [1])


(*----------------------------------------------------------------------------------
val factorial1 = fn : int list -> int list
working factorial function on int list
------------------------------------------------------------------------------------*)
fun factorial1(l: int list) = 
	(*
		Base case: is list is [0] or [1] then return [1]
	*)
	if(isLessThanEqualTo1(l)) then [1]

	(*
		do recursion for factorial : l * fact(l-1)
		decreaseBy1 function decreases no. by 1
	*)
	else 
		karatsuba l (factorial1( (decreaseBy1 l) ))


(*----------------------------------------------------------------------------------
val factorial1_tr = fn : int list * int list -> int list
Tail-recursive factorial function on int list
This function takes longer time than factorial1. So this function is not used
------------------------------------------------------------------------------------*)
(*fun factorial1_tr(l: int list, result: int list) = 
	if(isEqualTo1(l)) then result
	else
		factorial1_tr((decreaseBy1 l), karatsuba l result)*)



(*----------------------------------------------------------------------------------
val factorial = fn : string -> string
Main function for calculating factorial of string
------------------------------------------------------------------------------------*)
fun factorial(s: string) = 
	(*
		check if input is valid or not
	*)
	if validateInput s then 	
		let
			(*
				convert input string to list and trim its leading 0's
				calculate factorial using factorial1 function
				convert the factorial output to string and trim string to remove leading 0's
			*)
			val l = trimList (fromString(s));
			val fact = factorial1(l);
			val result = trimString (toString(fact));
		in
			result
		end
	(*
		for invalid input raise exception
	*)
	else 
		raise Invalid_Input_exception "Invalid input"


(*----------------------------------------------------------------------------------
val factorialbasic = fn : int -> int
Basic factorial function on integer
This function is not used
------------------------------------------------------------------------------------*)
(*fun factorialbasic(n: int) = 
	if(n <= 1) then
		1
	else
		n*factorialbasic(n-1)
*)


(*----------------------------------------------------------------------------------
val factorialbasic_tr = fn : int * int -> int
Basic tail recursive factorial function on integer
This function is not used
------------------------------------------------------------------------------------*)
(*fun factorialbasic_tr(n: int, result: int) = 
	if(n <= 1) then
		result
	else
		factorialbasic_tr(n-1, n*result)

*)
			
