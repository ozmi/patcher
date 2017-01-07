package object patcher {

    def diff [T, P <: Patch [T]] (from : T, to : T)(implicit patcher : Patcher [T, P]) : Option [P] =
        patcher.calculate (from, to)

    def patch [T, P <: Patch [T]] (from : T, patch : P)(implicit patcher : Patcher [T, P]) : T =
        patcher.apply (from, patch)

}
