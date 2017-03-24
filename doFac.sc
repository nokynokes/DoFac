def doFac(n: Int): DoWith[Map[Int,BigInt], BigInt] = n match {
    //Base Case will return 1
    case 1 => doreturn(1)
      // Either the function call is cached (i.e. the function call was already made)
      // or it is brand new
    case _ => doget flatMap { Mem => //doget grabbed our state
      try {
        doreturn(Mem(n))
      } catch {
        case _ : NoSuchElementException => doFac(n - 1) flatMap {
          result =>
            val ans = n * result
            domodify{ m: Map[Int,BigInt] => m + (n -> ans) } map { u => ans}
            //doput(Mem + (n -> ans)) map {_ => ans}
        }
      }
    }
  }
}
val n = new Factorials()
 /* lets compute 5 fac and then 10 fac */

val (m, r) = n.doFac(5)(Map.empty)

//n.doFac(15)(n.doFac(10)(m)._1)
