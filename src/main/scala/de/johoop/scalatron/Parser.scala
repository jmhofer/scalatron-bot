package de.johoop.scalatron

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.valid
import cats.instances.all._
import cats.syntax.cartesian._
import cats.syntax.traverse._
import de.johoop.scalatron.model._

import scala.collection.breakOut
import scala.util.Try

object Parser {
  def parseServerCommand(in: String): WithFailures[ServerCommand] = in match {
    case CommandRE(name, params) => parseParams(name, params) andThen commandFromOpcodeAndMap(name)
    case other                   => invalid(s"unable to parse command: '$other'")
  }

  case class Failure(msg: String) extends AnyVal

  type WithFailures[T] = Validated[NonEmptyList[Failure], T]

  private val CommandRE = """(.+)\((.*)\)""".r

  private[scalatron] def commandFromOpcodeAndMap(name: String)(params: String Map String): WithFailures[ServerCommand] = name match {
    case "Welcome" =>
      param(params, "name") |@|
        intParam(params, "apocalypse") |@|
        intParam(params, "round") |@|
        optIntParam(params, "maxslaves") map Welcome

    case "Goodbye" => intParam(params, "energy") map Goodbye

    case "React" =>
      param(params, "name") |@|
        intParam(params, "generation") |@|
        intParam(params, "time") |@|
        viewParam(params, "view") |@|
        optXYParam(params, "master") |@|
        optXYParam(params, "collision") |@|
        optIntParam(params, "slaves") map React

    case other => invalid(s"unknown opcode: '$other'")
  }

  private[scalatron] def parseParams(name: String, params: String): WithFailures[String Map String] =
    if (params.isEmpty) valid(Map.empty)
    else params.split(",", -1).toList.traverseU[WithFailures[(String, String)]] { parseParam(name) } map (_.toMap)

  private[scalatron] def parseParam(name: String)(param: String): WithFailures[(String, String)] = param.split("=", -1) match {
    case Array(key, value) => valid((key, value))
    case _                 => invalid(s"unable to parse parameter for command '$name': '$param'")
  }

  private[scalatron] def param(params: String Map String, name: String): WithFailures[String] =
    Validated.fromOption(params get name, NonEmptyList.of(Failure(s"no parameter named '$name' found")))

  private[scalatron] def optParam(params: String Map String, name: String): WithFailures[Option[String]] = valid(params.get(name))

  private[scalatron] def intParam(params: String Map String, name: String): WithFailures[Int] = param(params, name) andThen intFromTry

  private[scalatron] def viewParam(params: String Map String, name: String): WithFailures[View] = param(params, name) andThen parseView

  private[scalatron] def xyParam(params: String Map String, name: String): WithFailures[XY] = param(params, name) andThen parseXY

  private[scalatron] def optIntParam(params: String Map String, name: String): WithFailures[Option[Int]] = optParam(params, name).andThen {
    case None => valid(None)
    case Some(p) => fromTry(s"not parsable as int: '$name'")(Some(p.toInt))
  }

  private[scalatron] def optXYParam(params: String Map String, name: String): WithFailures[Option[XY]] = optParam(params, name).andThen {
    case None => valid(None)
    case Some(p) => parseXY(p) map (Some(_))
  }

  private[scalatron] def parseXY(in: String): WithFailures[XY] = in.split("\\:", -1) match {
    case Array(x, y) => intFromTry(x) |@| intFromTry(y) map XY
    case _ => invalid(s"not parsable as XY: '$in'")
  }

  private[scalatron] def parseView(in: String): WithFailures[View] =
    if (in.length != View.size) invalid(s"view has to have size ${View.size}")
    else fromTry(s"unable to parse view: '$in'") { View(in.map(parseCell)(breakOut[String, Cell, Vector[Cell]]).grouped(View.edgeSize).toVector) }

  private[scalatron] def parseCell(c: Char): Cell = Cell.withValue(c) // throws, keeping that for performance reasons

  private def intFromTry(str: String) = fromTry(s"not parsable as int: '$str'")(str.toInt)

  private def fromTry[T](msg: String)(f: => T) = Validated.fromTry(Try(f)).leftMap(_ => NonEmptyList.of(Failure(msg)))

  private def invalid(msg: String) = Validated.invalid(NonEmptyList.of(Failure(msg)))
}
