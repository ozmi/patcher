package delta

trait Patch [T] {

    def isEmpty : Boolean

    def invert : Patch [T]

    def applyTo (from : T) : T

}

object Patch {

    def empty [T] : Patch [T] = new Patch [T] {
        override val isEmpty : Boolean = true
        override val invert : Patch[T] = this
        override def applyTo (from : T) : T = from
    }

    case class Full [T] (from : T, to : T) extends Patch [T] {
        override lazy val isEmpty : Boolean = from == to
        override lazy val invert : Patch [T] = Full (to, from)
        override def applyTo (from : T) : T = to
    }

    implicit class RichPatch [T] (val patch : Patch [T]) extends AnyVal {
        def minimize : Patch [T] =
            if (patch.isEmpty) {
                Patch.empty
            } else {
                patch
            }
    }

}
