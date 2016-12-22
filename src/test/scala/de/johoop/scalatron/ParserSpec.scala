package de.johoop.scalatron

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.valid
import de.johoop.scalatron.Parser.Failure
import de.johoop.scalatron.model._
import org.specs2.Specification
import org.specs2.matcher.DataTables

// TODO view parsing
class ParserSpec extends Specification with DataTables { def is = "Server Command Parser".title ^ s2"""
  The server command parser parses command lists received from the server as input to the bot.

  It parses
    X/Y coordinates,              $xy
    individual string parameters, $param
    parameters, and               $params
    server commands               $command
  properly.
  """

  def xy =
    "input" || "output"                                                         |>
    "3:-4"  !! valid(XY(3, -4))                                                 |
    "0:0"   !! valid(XY(0, 0))                                                  |
    "a:5"   !! invalid("not parsable as int: 'a'")                              |
    "-1:b"  !! invalid("not parsable as int: 'b'")                              |
    ":-123" !! invalid("not parsable as int: ''")                               |
    "12:"   !! invalid("not parsable as int: ''")                               |
    ":"     !! invalid("not parsable as int: ''", "not parsable as int: ''")    |
    "meep"  !! invalid("not parsable as XY: 'meep'")                            |
    "1::3"  !! invalid("not parsable as XY: '1::3'")                            |
    "1:3:0" !! invalid("not parsable as XY: '1:3:0'")                           |
      { (in, out) => Parser.parseXY(in) must_=== out }

  def param =
    "input" || "output"                                                         |>
    "k=str" !! valid(("k", "str"))                                              |
    "key="  !! valid(("key", ""))                                               |
    "k=v="  !! invalid("unable to parse parameter for command 'cmd': 'k=v='")   |
      { (in, out) => Parser.parseParam("cmd")(in) must_=== out }

  def params =
    "input"             || "output"                                                       |>
    "k=str,foo=bar,a=3" !! valid(Map("k" -> "str", "foo" -> "bar", "a" -> "3"))           |
    ""                  !! valid(Map.empty[String, String])                               |
    "k=str,b=v=,b=45"   !! invalid("unable to parse parameter for command 'cmd': 'b=v='") |
      { (in, out) => Parser.parseParams("cmd", in) must_=== out }

  def command =
    "input"                                                              || "output"                                                                     |>
    "Goodbye(energy=-999)"                                               !! valid(Goodbye(-999))                                                         |
    "Welcome(name=meep,apocalypse=123,round=0,unknown=meep)"             !! valid(Welcome("meep", 123, 0, None))                                         |
    "Welcome(name=foo,apocalypse=-99,round=100,maxslaves=3)"             !! valid(Welcome("foo", -99, 100, Some(3)))                                     |
    s"React(name=,generation=1,time=-1,view=$emptyViewRaw,master=-1:-2)" !! valid(React("", 1, -1, emptyView, Some(XY(-1, -2)), None, None))             |
    "Invalid(name=foo)"                                                  !! invalid("unknown opcode: 'Invalid'")                                         |
    "(name=foo)"                                                         !! invalid("unable to parse command: '(name=foo)'")                             |
    "Goodbye()"                                                          !! invalid("no parameter named 'energy' found")                                 |
    "Welcome(name=foo,round=a)"                                          !! invalid("no parameter named 'apocalypse' found", "not parsable as int: 'a'") |
      { (in, out) => Parser.parseServerCommand(in) must_=== out }

  private def invalid(head: String, tail: String*) = Validated.invalid(NonEmptyList.of(Failure(head), tail map Failure :_*))

  def emptyView = View(Vector.fill(View.edgeSize)(Vector.fill(View.edgeSize)(Cell.Empty)))
  def emptyViewRaw = Vector.fill(View.size)(Cell.Empty.value).mkString
}
