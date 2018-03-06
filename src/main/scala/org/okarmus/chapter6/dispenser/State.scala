package org.okarmus.chapter6.dispenser

case class MachineState[S, +A](run: S => (A, S)) {

}


object MachineState {

}
