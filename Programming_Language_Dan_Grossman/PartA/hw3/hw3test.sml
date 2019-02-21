(* Homework3 Simple Test*)
(* These are basic test cases.
Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["A","bc","C","ad","ad"] = "bc"
val test3_1 = longest_string2 ["A","bc","C","ac","ad"] = "ad"
val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a_1 = longest_string3 ["A","bc","C","ad","ad"] = "bc"
val test4b = longest_string4 ["A","B","C"] = "C"
val test4b_1 = longest_string4 ["A","bc","C","ac","ad"] = "ad"
val test5 = longest_capitalized ["A","bc","C"] = "A"
val test6 = rev_string "abc" = "cba"
val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [2] = SOME [2]
val test8_2 = all_answers (fn x => if x > 3 then SOME [x] else NONE) [4,5,6,7,8] = SOME [4,5,6,7,8]
val test9a = count_wildcards Wildcard = 1
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_1 = count_wild_and_variable_lengths (ConstructorP("hi",TupleP[Wildcard,Variable("a"),Variable("asd")])) =5
val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("x", (ConstructorP("x",TupleP[Wildcard,Variable("a"),Variable("x")]))) = 2
val test10 = check_pat (Variable("x")) = true
val test10_1 = check_pat (ConstructorP("z",TupleP[Wildcard,Variable("x"),Variable"y",Variable"z"])) = false
val test11 = match (Const(1), UnitP) = NONE
val test11_1 = match (Tuple[Const(1),Unit,Unit],TupleP[Variable"hi",Wildcard,Variable"world"]) = SOME [("hi",Const 1),("world",Unit)]
val test11_2 = match (Tuple[Const(1),Unit,Unit,Unit,Const 5],TupleP[Variable"hi",Wildcard,Variable"world"]) = NONE
val test11_3 = match (Constructor("we",Tuple[Const(1),Unit,Unit]),ConstructorP("we",TupleP[Variable"hi",Wildcard,Variable"world"])) = SOME [("hi",Const 1),("world",Unit)]
val test12 = first_match Unit [UnitP] = SOME []
val test12_1 = first_match Unit [ConstructorP("hi",Wildcard),TupleP[Variable"world",UnitP],UnitP,Variable"hi"] = SOME []
val test12_2 = first_match Unit [ConstructorP("hi",Wildcard),TupleP[Variable"world",UnitP],Variable"hi",UnitP] = SOME [("hi",Unit)]
