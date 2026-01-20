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
    var A_ray = Array.range(0, 9)
    var B_ray = Array(1, 2, 4, 5, 7, 0, 8, 6, 3)
    var C_ray = Array.range(8, -1, -1)

    def highar(per1: Array[Int], per2: Array[Int]): Boolean =
      if maxSort(per1) sameElements per2 then
        false
      else
        var max_ind = per2.length - 1
        var temp = 1
        while per2(max_ind) < per2(max_ind - 1) do
          max_ind = max_ind - 1
        temp = per2(max_ind)
        per2(max_ind) = per2(max_ind - 1)
        per2(max_ind - 1) = temp
        true

    println(highar(A_ray, B_ray))
    for i <- maxSort(A_ray) do
      print(A_ray(i))


  }
}