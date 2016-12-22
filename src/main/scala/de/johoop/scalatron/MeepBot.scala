package de.johoop.scalatron

class MeepBot {
  def step(in: String): String = Parser.parseServerCommand(in).fold({ f =>
    val failures = f.toList map (_.msg) map escape mkString "\n"
    s"Log(text=$failures)|Status(text=FAILED)"
  }, _ => "Status(text=OK)" )

  def escape(msg: String): String =
    List(("=", "≅"), ("\\(", "{"), ("\\)", "}"), ("\\|", "∣"), (",", ";")).foldLeft(msg) { case (m, (p, s)) => m.replaceAll(p, s) }
}
