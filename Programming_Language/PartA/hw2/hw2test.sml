(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val testq1a = all_except_option ("string", ["string"]) = SOME []

val testq1a_1 = all_except_option("op",["1","2","3"]) = NONE

val testq1b = get_substitutions1 ([["foo"],["there"]], "foo") = []

val testq1b = get_substitutions1 ([["foo","1","2"],["there","1","foo"],["1"]], "foo") = ["1","2","there","1"]

val testq1b = get_substitutions1 ([["foo"],["there"]], "fo") = []								    

val testq1c = get_substitutions2 ([["foo"],["there"]], "foo") = []

val testq1c = get_substitutions2 ([["foo","1","2"],["there","1","foo"],["1"]], "foo") = ["1","2","there","1"]

val testq1c = get_substitutions2 ([["foo"],["there"]], "fo") = []								    

val testq1d = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]


val testq1d_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Betty", middle="W", last="Smith"}) =
		[{first="Betty", last="Smith", middle="W"},
		 {first="Elizabeth", last="Smith", middle="W"}]

		    



		    
val testq2a = card_color (Clubs, Num 2) = Black

val testq2b = card_value (Clubs, Num 2) = 2

val testq2c = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val testq2d = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val testq2e = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val testq2f = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val testq2f_1 = score ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2

val testq2g = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val testq2g = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val testq2g = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)


val testq2g_1 = officiate ([(Clubs,Jack),(Spades,King),(Clubs,King),(Spades,Ace)],
                         [Draw,Draw,Discard(Clubs,Jack),Draw],
                         24)
		= 2
val testq2g_2 = officiate ([(Clubs,Jack),(Spades,King),(Clubs,King),(Spades,Ace),(Hearts, Num 2),(Hearts, Num 4),(Hearts, Num 8)],[Draw,Draw,Discard(Clubs,Jack),Draw,Draw,Draw,Draw,Discard(Spades,Ace)],
                         38)
			  = 12
