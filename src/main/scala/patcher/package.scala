package object patcher {

    def calculatePatch [T, P <: Patch [T]] (from : T, to : T) (implicit patcher : Patcher [T, P]) : P =
        patcher.calculate (from, to)

}
