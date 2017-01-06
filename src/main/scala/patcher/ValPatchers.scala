package patcher

trait ValPatchers {

    case class BooleanPatch (isEmpty : Boolean) extends Patch [Boolean] {
        override val invert : Patch [Boolean] = this
    }

    implicit object BooleanPatcher extends Patcher [Boolean, BooleanPatch] {
        override def calculate (from : Boolean, to : Boolean) : BooleanPatch = BooleanPatch (to == from)
        override def apply (from : Boolean, patch : BooleanPatch) : Boolean = if (patch.isEmpty) from else !from
    }

    case class IntPatch (delta : Int) extends Patch [Int] {
        override lazy val isEmpty : Boolean = delta == 0
        override lazy val invert : Patch [Int] = IntPatch (- delta)
    }

    implicit object IntPatcher extends Patcher [Int, IntPatch] {
        override def calculate (from : Int, to : Int) : IntPatch = IntPatch (to - from)
        override def apply (from : Int, patch : IntPatch) : Int = from + patch.delta
    }

}

object ValPatchers extends ValPatchers {

    val patch = calculatePatch (true, false)

}
