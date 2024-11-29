val validTriangle = (a:Int, b:Int, c:Int) => a+b>c && b+c>a && a+c>b

val inputLines = scala.io.Source.fromFile("/home/jaysicks/tmp/advent-of-code/src/main/resources/aoc2016/day02.txt").getLines().toList

val triangles = inputLines.map(_.trim.split(" +")).map{ case Array(a, b, c) => (a.toInt, b.toInt, c.toInt) }

println(triangles.count(validTriangle.tupled))

val (col1,col2,col3) = triangles.unzip3

val triangles2 = (col1 ++ col2 ++ col3).grouped(3).toList.map{ case List(a, b, c) => (a.toInt, b.toInt, c.toInt) }

println(triangles2.count(validTriangle.tupled))