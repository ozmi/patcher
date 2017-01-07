package patcher

trait ValPatchers {

    case class BooleanPatch (isEmpty : Boolean) extends Patch [Boolean] {
        override val invert : Patch [Boolean] = this
    }

    implicit object BooleanPatcher extends Patcher [Boolean, BooleanPatch] {
        override def calculate (from : Boolean, to : Boolean) : Option [BooleanPatch] = BooleanPatch (to == from).minimize
        override def apply (from : Boolean, patch : BooleanPatch) : Boolean = if (patch.isEmpty) from else !from
    }

    case class IntPatch (delta : Int) extends Patch [Int] {
        override lazy val isEmpty : Boolean = delta == 0
        override lazy val invert : Patch [Int] = IntPatch (- delta)
    }

    implicit object IntPatcher extends Patcher [Int, IntPatch] {
        override def calculate (from : Int, to : Int) : Option [IntPatch] = IntPatch (to - from).minimize
        override def apply (from : Int, patch : IntPatch) : Int = from + patch.delta
    }

    implicit def optionPatcher [T, P <: Patch [T]] (implicit valuePatcher : Patcher [T, P]) : OptionPatcher [T, P] =
        OptionPatcher (valuePatcher)

}

object ValPatchers extends ValPatchers {

    val patch = diff (Option (true), Option (false)) (optionPatcher (BooleanPatcher))

    println (patch)

}
