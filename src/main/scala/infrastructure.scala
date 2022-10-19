package yedis


extension [A, B] (x: => A) def |> (f: A => B): B = f(x)

extension [A] (xs: => Seq[Option[A]]) 
  def sequence: Option[Seq[A]] =
    xs.foldLeft(Option(Seq.empty[A])) {
      case (Some(z), Some(a)) => Option(a +: z)
      case otherwise          => Option.empty
    }

trait Read[A]:
  def parse(s: String): Option[A]

object Read:
  def apply[A](using Read[A]) = summon[Read[A]]

  def make[A](f: String => Option[A]) = new Read[A]:
    def parse(s: String): Option[A] = f(s)

extension [A] (x: => String) 
  def as(using Read[A]): Option[A] =
    Read[A].parse(x)


trait Show[A]:
  def show(x: A): String

object Show:
  def apply[A](using Show[A]) = summon[Show[A]]

  def make[A](f: A => String) = new Show[A]: 
    def show(x: A) = f(x)

extension [A] (x: => A) 
  def show[B >: A](using Show[B]): String =
    Show[B].show(x)


import model.Protocol as P

trait Encoder[A]:
  def encode(send: A): P.Value

object Encoder:
  def apply[A](using Encoder[A]) = summon[Encoder[A]]

  def asInt(x: P.Value) = x match
    case P.Value.Integer(x) => Option(x)
    case otherwise          => Option.empty

  def make[A, B](send: A => P.Value) = new Encoder[A]:
    def encode(x: A) = send(x)

trait Decoder[A]:
  def decode(recv: P.Value): Option[A]

object Decoder:
  def apply[A](using Decoder[A]) = summon[Decoder[A]]

  def make[A](recv: P.Value => Option[A]) = new Decoder[A]:
    def decode(x: P.Value) = recv(x)