
fun n_times(f,n,x) =
    if n = 0
    then x
    else f(n_times(f,n-1,x))

fun triple x = 3*x

fun double x = x + x
fun tri_n_times(n,x) =
    n_times((fn x => 3*x),n,x)

fun compose(f,g) = fn x => f(g x)


infix !>
fun x !> f = f x
			    
fun triple_double x= (triple o double) x

fun triple_double_3 x = compose(triple,double) x
fun triple_double_4 x = x !> double !> triple




val triple_double_1 = compose(triple,double)
val triple_double_2 = triple o double



fun backup1 (f,g) = fn x =>
		      case f x of
			  NONE => g x
		       | SOME y => y

fun backup2 (f,g) = fn x => f x handle _ => g x



fun sorted3_tupled(x,y,z) = z >= y andalso y >= x
val t1 = sorted3_tupled(7,9,11)


val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

val t2 = (((sorted3 7) 9) 11)
val t3 = sorted3 7 9 11
							      
fun sorted3_nicer x y z = z >= y andalso y >= x
