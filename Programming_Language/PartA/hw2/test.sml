fun same_string(s1 : string, s2 : string) =
    s1 = s2


fun get_substitutions1 (LL,s) =
    let fun q1_helper (L,s) =
	    case(L,s)of
		([],_) => []
	      | (L1::L',s) => if same_string(s,L1) then L' else L1::(q1_helper(L',s))
    in case (LL,s) of
	   ([],s) => []
	 | (LL1::LL',s) => if q1_helper(LL1,s)=LL1
			   then get_substitutions1(LL',s)
			   else q1_helper(LL1,s)@get_substitutions1(LL',s)
    end

fun get_substitutions2 (LL,s) =
    let fun h2 (LL,s,ys)=
	    let fun q1_helper (L,s) =
		    case(L,s)of
			([],_) => []
		      | (L1::L',s) => if same_string(s,L1) then L' else L1::(q1_helper(L',s))
	    in case (LL,s,ys) of
		   ([],s,ys) => ys
		 | (LL1::LL',s,ys) => if q1_helper(LL1,s)=LL1
				      then h2(LL',s,ys)
				      else h2(LL',s,ys@q1_helper(LL1,s))
	    end
    in h2(LL,s,[])
    end

fun similar_names (LL,r) = 
    let fun helper (L,r) =
	    case (L,r) of
		([],{first=x,middle=y,last=z}) => []
	      | (L1::L',{first=x,middle=y,last=z}) => {first=L1,last=z,middle=y}::helper(L',r)
    in case (LL,r) of
	   ([], {first=x,middle=y,last=z}) => [{first=x,last=z,middle=y}]
	 | (LL1::LL',{first=x,middle=y,last=z}) => {first=x,last=z,middle=y}::helper(get_substitutions2(LL,x),r)
    end
