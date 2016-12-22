package scalatron.botwar.botPlugin

import de.johoop.scalatron.MeepBot

class ControlFunctionFactory {
  def create: String => String = {
    val bot = new MeepBot
    bot.step _
  }
}
