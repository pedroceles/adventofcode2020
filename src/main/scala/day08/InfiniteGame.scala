package day08

import utils.BaseApp

trait Instruction {
  val arg: Int
  def nextGameState(state: GameState): GameState
}
case class NOp(arg: Int) extends Instruction {
  def nextGameState(state: GameState): GameState = state.copy(instructionPointer = state.instructionPointer + 1)
}
case class Jump(arg: Int) extends Instruction {
  def nextGameState(state: GameState): GameState = state.copy(instructionPointer = state.instructionPointer + arg)
}
case class Acc(arg: Int) extends Instruction {
  def nextGameState(state: GameState): GameState = state.copy(state.acc + arg, state.instructionPointer + 1)
}
object Instruction {
  def apply(string: String): Instruction = string match {
    case s"${instruction} ${arg}" => {
      val intArg = arg.toInt
      instruction match {
        case "nop" => NOp(intArg)
        case "jmp" => Jump(intArg)
        case "acc" => Acc(intArg)
      }
    }
  }
}
case class GameState(acc: Int, instructionPointer: Int)
case class Game(state: GameState, instructions: Seq[Instruction], playedInstructionIndex: Set[Int] = Set()) {
  lazy val instruction = instructions(state.instructionPointer)
  lazy val isTerminated = state.instructionPointer == instructions.length
  def playUntilInfiniteLoop: Game = {
    val instructionAlreadyPlayed = playedInstructionIndex contains state.instructionPointer
    if(isTerminated || instructionAlreadyPlayed) this
    else {
      val newSet = playedInstructionIndex + state.instructionPointer
      val newState = instruction.nextGameState(state)
      Game(newState, instructions, newSet).playUntilInfiniteLoop
    }
  }
}

object App extends BaseApp[Int, Int] {
  lazy val instructions = getLines.map(Instruction(_)).toVector
  lazy val game = Game(GameState(0, 0), instructions)
  override def partI(): Int = {
    game.playUntilInfiniteLoop.state.acc
  }
  override def partII(): Int = {
    val infiniteGame = game.playUntilInfiniteLoop
    infiniteGame.playedInstructionIndex.foldLeft(0)((acc, index) => {
      val instruction = instructions(index)
      val newInstructions = instruction match {
        case NOp(arg) =>  instructions updated(index, Jump(arg))
        case Jump(arg) =>  instructions updated(index, NOp(arg))
        case _ => instructions
      }
      if(newInstructions == instructions) acc
      else {
        val newGame = Game(GameState(0, 0), newInstructions)
        val endGame = newGame.playUntilInfiniteLoop
        if(endGame.isTerminated) endGame.state.acc
        else acc
      }

    })
  }
}
