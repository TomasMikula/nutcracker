package object nutcracker {

  type ObserveVal[M[_], Val0[_]] = Observe[M] { type Val[A] = Val0[A] }

}
