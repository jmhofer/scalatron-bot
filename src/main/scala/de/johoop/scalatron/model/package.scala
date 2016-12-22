package de.johoop.scalatron

package object model {
  sealed abstract class ServerCommand(val opcode: String)

  case class Welcome(name: String, apocalypse: Int, round: Int, maxSlaves: Option[Int]) extends ServerCommand("Welcome")
  case class Goodbye(energy: Int) extends ServerCommand("Goodbye")
  case class React(name: String, generation: Int, time: Int, view: View, master: Option[XY], collision: Option[XY], slaves: Option[Int])
    extends ServerCommand("React")

  case class XY(x: Int, y: Int)

  case class View(cells: Vector[Vector[Cell]]) {
    override def toString = cells flatMap (_ map (_.value)) mkString
  }

  object View {
    val edgeSize = 31
    val size = edgeSize * edgeSize
  }

  import enumeratum.values._

  sealed abstract class Cell(val value: Char) extends CharEnumEntry

  case object Cell extends CharEnum[Cell] {
    case object Occluded extends Cell('?')
    case object Wall extends Cell('W')
    case object Empty extends Cell('_')
    case object Zugar extends Cell('P')
    case object Toxifera extends Cell('p')
    case object MyMaster extends Cell('M')
    case object OtherMaster extends Cell('m')
    case object MySlave extends Cell('S')
    case object OtherSlave extends Cell('s')
    case object Fluppet extends Cell('B')
    case object Snorg extends Cell('b')

    def values = findValues
  }
}
