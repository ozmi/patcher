package patcher

sealed trait OptionPatch [T] extends Patch [Option [T]]

object OptionPatch {

    case class SomeToNone [T] (from : Option [T]) extends OptionPatch [T] {
        override val isEmpty : Boolean = false
        override def invert : Patch [Option[T]] = NoneToSome (from)
    }

    case class NoneToSome [T] (to : Option [T]) extends OptionPatch [T] {
        override val isEmpty : Boolean = false
        override def invert : Patch [Option[T]] = SomeToNone (to)
    }

}

class OptionPatcher {

}
