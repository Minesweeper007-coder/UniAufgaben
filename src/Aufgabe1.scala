object Aufgabe1 {
  def main(args: Array[String]): Unit = {

    //Aufgabe a)
    val zahlen = Array(4, 2, 7, 0, 1, 5, 3, 6, 3)

    def isPerm(p: Array[Int]): Boolean =
      val n = p.length
      var cfm = Array.fill(n)(false)
      if n > 9 then
        false
      else
        for i <- 0 to 8 do
          if p.contains(i) then
            cfm(i) = true
      if cfm.contains(false) then false
      else
        true

    // Aufgabe b) Absteigend Sortiert

    def maxSort(arr: Array[Int]): Array[Int] =
      val n = arr.length
      // Wir gehen von hinten nach vorne durch das Array
      // (i ist die Position, an die das Maximum geschoben werden soll)
      for i <- (n - 1) to 1 by -1 do
        var maxIdx = 0
        // Suche den Index des größten Elements im Bereich [0 bis i]
        for j <- 1 to i do
          if arr(j) > arr(maxIdx) then
            maxIdx = j
        // Tausche das gefundene Maximum mit dem Element an Position i
        val temp = arr(maxIdx)
        arr(maxIdx) = arr(i)
        arr(i) = temp
      arr

    // Aufgabe c) die beiden Permutationen folgen aufeinander, weil die erste bereits die Höchste zahl ohne verschiebung der
    // höheren Zahl darstellt

    // Aufgabe d) 032168754

    // Fast komplett e)
    var A_ray = Array.range(0, 9)
    var B_ray = Array(1, 2, 4, 5, 7, 0, 8, 6, 3)
    var C_ray = Array.range(8, -1, -1)

    def highar(per1: Array[Int], per2: Array[Int]): Boolean =
      if per2 sameElements maxSort(per2) then
        false
      else
        var max_ind = per2.length - 1
        var temp = 1
        while max_ind >= 2 && per2(max_ind) < per2(max_ind - 1) do
          max_ind = max_ind - 1
        temp = per2(max_ind)
        per2(max_ind) = per2(max_ind - 1)
        per2(max_ind - 1) = temp
        true

    //Aufgabe 2 a) s. Blatt
    // 2b)
    type Perm = Array[Int]
    type Cycles = List[List[Int]]

    def cyclesOf(p: Perm): Cycles =
      val n = p.length
      var cs: Cycles = List()
    // Array zum Merken, welche Zahl schon vorkam
      var cfm = Array.fill(n)(false)
      for i <- 0 to n - 1 do
          if !cfm(i) then
            cfm(i) = true
            var cycle = List(i)
            // Zyklusanfang
            var j = p(i)
            // Zyklus durchgehen
            while j != i do
              cfm(j) = true
              cycle = cycle ::: List(j)
              j = p(j)
              cs = cs ::: List(cycle)
      return cs

    def printCycles(p: Perm): Unit =
      for c <- cyclesOf(p)
        do println(c)

    var D_ray: Array[Int] = Array.range(0,9)
    var E_ray: Array[Int] = Array(0,3,2,1,5,8,7,6,4)
    var F_ray: Array[Int] = Array(0,3,5,1,8,6,2,4,7)


    // Einzeln stehende Gruppen werden nicht ausgegeben


    var testo: Cycles = List(List(1,3),List(4,5),List(4,5,8),List(6,7))

    def clean(cs: Cycles): Cycles =
      cs.filter { l =>
        val count = cs.count(e => e.containsSlice(l))
        count < 2
      }

    def permByCycles(cs: Cycles): Perm =
      var temp:Int = 0
      def maxEl(cs: Cycles): Int =
        var max: Int = 0
        for l <- cs do
          for e <- l do
            if e > max then
              max = e
        return max
      var p = Array.range(0,maxEl(cs)+1)
      for element <- cs do
        var len:Int = element.length
        val first:Int = p(element.head)
        var i = 0
        while i < len -1 do
          p(element(i)) = p(element(i+1))
          i += 1
        p(element.last) = first
      return p
/*
    for i <- permByCycles(clean(cyclesOf(E_ray))) do
      print(i)
*/
    // Aufgabe 3a)
    val Ub_ray: Array[Int] = Array(0, 1, 2, 3, 4, 5, 6, 7, 8)
    val P = Array(7, 3, 0, 2, 5, 1, 6, 4)
    val Q = Array(5, 2, 4, 6, 0, 3, 1, 7)

    def spiegleF(p: Perm): Perm =
      var res = Array.fill(8)(0)
      for i <- 0 to 7 do
        res(p(i)) = i
      return res

    for i <- spiegleF(Q) do
      print(i)

    // Aufgabe 3b)
    def lexKleiner(p:Perm,q:Perm):Boolean =
      if p.mkString.toInt > q.mkString.toInt then
        true
      else
        false



  }
}