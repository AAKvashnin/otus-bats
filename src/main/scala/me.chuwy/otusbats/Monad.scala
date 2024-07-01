package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(fa=>fa)
}

object Monad {

 implicit def optionMonad(implicit fun:Functor[Option]):Monad[Option] = new Monad[Option] {

   override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
     case None => None
     case Some(a)=>Some(f(a))
   }

   override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fun.map(fa)(f).flatten

   override def point[A](a:A):Option[A]=Some(a)


 }

 implicit def listMonad(implicit fun:Functor[List]):Monad[List] = new Monad[List] {

   override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

   override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fun.map(fa)(f).flatten

   override def point[A](a:A):List[A]=List(a)

 }

}
