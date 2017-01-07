package patcher

import patcher.OptionPatch._

sealed trait OptionPatch [T] extends Patch [Option [T]]

object OptionPatch {

    case class SomeToNone [T] (from : T) extends OptionPatch [T] {
        override val isEmpty : Boolean = false
        override def invert : Patch [Option [T]] = NoneToSome (from)
    }

    case class NoneToSome [T] (to : T) extends OptionPatch [T] {
        override val isEmpty : Boolean = false
        override def invert : Patch [Option [T]] = SomeToNone (to)
    }

    case class SomeToSome [T] (from : T, to : T) extends OptionPatch [T] {
        override lazy val isEmpty : Boolean = from == to
        override def invert : Patch [Option[T]] = SomeToSome [T] (to, from)
    }

}

case class OptionPatcher [T, VP <: Patch [T]] (valuePatcher : Patcher [T, VP]) extends Patcher [Option [T], OptionPatch [T]] {

    override def calculate (from : Option [T], to : Option [T]) : Option [OptionPatch [T]] =
        (from, to) match {
            case (Some (f), Some (t)) =>
                SomeToSome (f, t).minimize
        }

    override def apply (from : Option [T], patch : OptionPatch [T]) : Option [T] = ???

}
