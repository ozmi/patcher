package patcher

trait Patcher [T, P <: Patch [T]] {

    def calculate (from : T, to : T) : Option [P]

    def apply (from : T, patch : P) : T

}