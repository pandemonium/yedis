package yedis
package model

import yedis.model.Protocol.Value


object Protocol:
  enum ErrorPrefix:
    case Empty, Err
    case Named(name: String)

  object ErrorPrefix:
    def make(prefix: String) = prefix match
      case "ERR"     => Err
      case otherwise => Named(otherwise)

  enum Value:
    case SimpleString(s: String)
    case Error(prefix: ErrorPrefix, message: String)
    case Integer(x: Long)
    case BulkString(s: String)
    case Array(xs: Seq[Value])
    case Nil

  object Value:
    def makeArray(xs: Value*) =
      if xs.isEmpty then Nil else Array(xs)

    def makeBulkString(size: Int, text: String) =
      if size == -1 then Nil else BulkString(text)

    def makeError(line: String) =
      val ix = line.indexOf(' ')
      if ix > -1 
        then Error(ErrorPrefix.make(line.substring(0, ix)), line.substring(ix).trim())
        else Error(ErrorPrefix.Empty, line.trim())

    def bulkArray(xs: String*) =
      xs.map(Value.BulkString(_)) |> Value.Array.apply

  private object Parser:
    import scala.collection.immutable.*

    enum Token:
      case Literal(value: String)
      case Trivial(value: Value)
      case BulkString(size: Int)
      case Array(length: Int)

    object Token:
      import Value.*, Token.*
      import scala.util.Try

      def trivial(v: Value) = Option(Trivial(v))

      def attempt[A](the: String, f: String => A) = Try(f(the.tail)).toOption

      def read(the: String): Option[Token] = the.head match
        case '+'       => trivial(SimpleString(the.tail))
        case '-'       => trivial(Value.makeError(the.tail))
        case ':'       => attempt(the, _.toLong) map (i => Trivial(Value.Integer(i)))
        case '*'       => attempt(the, _.toInt) map (Array(_))
        case '$'       => attempt(the, _.toInt) map (BulkString(_))
        case otherwise => Option.empty

    def read(text: String): Option[Value] =
      @annotation.tailrec
      def parseArray(
        count: Int,
        input: Seq[Token],
        output: Seq[Value]
      ): (Option[Seq[Value]], Seq[Token]) =
        if count == 0 then (Option(output), input)
        else parse(input) match
          case (Some(element), remaining) =>
            parseArray(count - 1, remaining, output :+ element)
          case diverged =>
            (Option.empty, input)

      def parse(input: Seq[Token]): (Option[Value], Seq[Token]) = input match
        case Token.Trivial(value)   +: tail =>
          (Option(value), tail)
        case Token.BulkString(size) +: Token.Literal(text) +: tail =>
          (Option(Value.makeBulkString(size, text)), tail)
        case Token.Array(length)    +: tail =>
          val (elements, remaining) = parseArray(length, tail, Seq.empty)
          (elements map Value.makeArray, remaining)
        case diverged =>
          (Option.empty, input)

      ArraySeq.unsafeWrapArray(text.split("\r\n"))
              .map(Token.read)
              .sequence
              .flatMap(parse(_).head)
              .headOption

  given Show[ErrorPrefix] with
    def show(x: ErrorPrefix) = x match
      case ErrorPrefix.Empty         => ""
      case ErrorPrefix.Err           => "ERR "
      case ErrorPrefix.Named(prefix) => s"${prefix.toUpperCase().trim()} "

  given Show[Value] with
    def show(the: Value) = the match
      case Value.SimpleString(text)     => s"+$text\r\n"
      case Value.Error(prefix, message) => s"-${prefix.show}$message\r\n"
      case Value.Integer(i)             => s":$i\r\n"
      case Value.BulkString(text)       => s"$$${text.length()}\r\n$text\r\n"
      case Value.Array(xs)              => s"*${xs.length}\r\n${xs.map(_.show).mkString}\r\n"
      case Value.Nil                    => s"$$-1\r\n"

  given Read[Value] with
    def parse(the: String): Option[Value] =
      Parser.read(the)

object DataType:
  import Protocol as P
  
  enum List:
    case Prepend(key: String, elements: String*)
    case Append(key: String, elements: String*)
    case Range(key: String, start: Int, stop: Int)
    case Length(key: String)

  enum SortedSet:
    case Add(key: String, score: Double, member: String)

  
  given Encoder[List] = Encoder.make(_ match
    case List.Prepend(key, elements*) => 
      Seq("LPUSH", key) ++ elements 
      |> P.Value.bulkArray
    case List.Append(key, elements*)  => 
      Seq("RPUSH", key) ++ elements 
      |> P.Value.bulkArray
    case List.Range(key, start, stop) => 
      Seq("LRANGE", key, start.toString, stop.toString)
      |> P.Value.bulkArray
    case List.Length(key) => 
      Seq("LRANGE", key)
      |> P.Value.bulkArray
  )

  given Encoder[SortedSet] = Encoder.make {
    case SortedSet.Add(key, score, member) =>
      Seq("ZADD", key, score.toString, member)
      |> P.Value.bulkArray
  }

  given Decoder[Int] = Decoder.make {
    case Value.Integer(x) => Option(x.toInt)
    case otherwise        => Option.empty
  }