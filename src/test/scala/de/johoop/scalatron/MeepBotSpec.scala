package de.johoop.scalatron

import org.specs2.Specification

class MeepBotSpec extends Specification { def is = "My Scalatron Bot".title ^ s2"""
  This is my Scalatron Bot.

  It must return a status message. $e1

  """.stripMargin

  def e1 = {
    val bot = new MeepBot
    bot.step("Goodbye(energy=1234)") must beMatching("""Status\(text=.*\)""")
  }
}
