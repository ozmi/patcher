package patcher

trait Patch [+T] {

    def isEmpty : Boolean

    def invert : Patch [T]

}

object Patch {

    val empty : Patch [Nothing] = new Patch [Nothing] {
        override val isEmpty : Boolean = true
        override val invert : Patch [Nothing] = this
    }

    case class Full [T] (from : T, to : T) extends Patch [T] {
        override lazy val isEmpty : Boolean = from == to
        override lazy val invert : Patch [T] = Full (to, from)
    }

    implicit class RichPatch [P <: Patch [_]] (val patch : P) extends AnyVal {
        def minimize : Option [P] =
            if (patch.isEmpty) {
                None
            } else {
                Some (patch)
            }
    }

}
