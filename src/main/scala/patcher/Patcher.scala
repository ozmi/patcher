package patcher

trait Patcher [T] {

    def calculate (from : T, to : T) : Patch [T]

    def apply (from : T, patch : Patch [T]) : T

}