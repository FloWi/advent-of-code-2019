package de.flwi.adventofcode.v2019

import de.flwi.adventofcode.v2019.Day9.myDebug

import scala.collection.mutable.ArrayBuffer


case class Amplifier(intcodeState: IntCodeComputer, phaseSetting: Int) {
  val outputValues: Vector[Long] = intcodeState.outputValues

  val currentOpCode: Long = intcodeState.opCode

  def run(input: Long, iteration: Int): Amplifier = {
    val inputValues = if (iteration == 0) Vector(phaseSetting, input) else Vector(input)
    val newState    = intcodeState.run(inputValues)

    Amplifier(newState, phaseSetting)
  }
}

object Amplifer {

  def runAmplifiers(intCodeProgram: Vector[Long], phaseSettings: Vector[Int]) = {
    def helper(amps: Vector[Amplifier], iteration: Int, inputForFirstAmp: Long): Vector[Amplifier] = {
      val amplifiers: ArrayBuffer[Amplifier] =
        collection.mutable.ArrayBuffer(amps: _*)

      def currentOpCodes = amplifiers.map(a => a.intcodeState.opCode)

      amplifiers.indices.foreach { idx =>
        if (currentOpCodes.contains(99)) {
          myDebug(s"iteration: $iteration; Amp #$idx; Found an amp that has halted. Opcodes of amps: ${currentOpCodes}. Continuing computation")
        }
        val amp = amplifiers(idx)
        val inputForCurrentAmp = if (idx == 0) inputForFirstAmp else amplifiers(idx - 1).outputValues.head

        val updatedAmp = amp.run(inputForCurrentAmp, iteration)
        amplifiers.update(idx, updatedAmp)
        //println(s"iteration: $iteration; Amp #$idx; input: $inputForCurrentAmp; result: ${updatedAmp.outputValues}. Current ip: ${updatedAmp.pointer} ")
      }

      val newAmps = amplifiers.toVector
      val last = newAmps.last
      if (last.currentOpCode == 99)
        newAmps
      else
        helper(newAmps, iteration + 1, last.outputValues.head)
    }

    val amps = helper(
      amps = phaseSettings.zipWithIndex.map {
        case (ps, idx) =>
          val initialState = IntCodeComputer(intCodeProgram, 0, Vector.empty, Vector.empty, relativeBase = 0, id = idx.toString)
          Amplifier(initialState, phaseSetting = ps)
      },
      iteration = 0,
      inputForFirstAmp = 0
    )

    myDebug(amps.mkString("\n"))
    amps.last
  }

  def feedbackLoopAmplifierProgram(prg: Vector[Long]) =
    9.to(5, step = -1)
      .toVector
      .permutations
      .map { phaseSettings =>
        val result = runAmplifiers(prg, phaseSettings)
        (result, phaseSettings)
      }
      .maxBy(_._1.outputValues.head)
}
