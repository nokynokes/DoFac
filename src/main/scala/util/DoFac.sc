import util.DoWith
import util.DoWith._

//import scala.collection.immutable.Map

/* Nolan Cretney
* Computing factorials using state monads
*
*
*
*
*/

def doFac(n: Int): DoWith[Map[Int,BigInt],BigInt] = n match {
  /* Base case will always return one */
  case 1 => doreturn(1)
  /* Either the function call is cached (i.e. the function call was already made)
  or it is brand new */
  case _ => doget flatMap { mem =>
    mem get n match {
      case Some(v) => doreturn(v)
      case None => doFac(n-1) flatMap { result =>
        val ans = n * result
        domodify {m: Map[Int,BigInt] => m + (n -> ans)} map {_ => ans}
      }
    }
  }
}

val computation = doFac(10) // We have a state monad waiting to compute 10!

computation(Map.empty) //We can get the computation by passing in an inital empty state
