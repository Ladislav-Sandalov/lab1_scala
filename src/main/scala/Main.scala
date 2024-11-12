object Main extends App {
  // 1.1 тест Hello world
  println("// 1.1")

  helloWorld 
  // 1.1

  // 1.2 many print hello world
  println("// 1.2")

  printHello(5)
  // 1.2

  // 1.3 use map, reduce, filter
  println("// 1.3")

  val nums = List(3, 5, 6, 13, 5, 4)
  println("Набор чисел " + nums)
  val (evenIndex, oddIndex) = splitIndex(nums) // use filter, map
  println("Четные индексы " + evenIndex)
  println("Нечетные индексы " + oddIndex)
  printfindMax(List(3, 5, 6, 13, 5)) // use reduce
  // 1.3

  // 1.4 Что же там лежит?
  println("// 1.4")

  println(printHello)
  println("Лямбда-функция, которая была сгенерирована компилятором / адрес объекта в памяти @ хэш-код объекта")
  // 1.4

  // 1.5 Использование pattern-matching
  println("// 1.5")

  describePoint(0, 0)
  describePoint(3, 0)
  describePoint(0, 2)
  describePoint(7, 1)
  // 1.5

  // 1.6 Два по цене одного
  println("// 1.6")

  println(compose(funcOne,funcTwo)(7))
  // 1.6
}

@main def hello() = println("hi") // 1.1 тест Hello world

def helloWorld = println("Hello, World! input lag")

// 1.2 & 1.4
def printHello(n: Int): Unit = {
  for (i <- 1 to n) {
    if (i % 2 == 0) {
      println(s"hello $i, # $i")
    } else {
      println(s"hello ${n - i}, # $i")
    }
  }
}
// 1.2 & 1.4

// 1.3
def splitIndex(nums: List[Int]): (List[Int], List[Int]) = {
  // Создаем индексированный список
  val indexNum = nums.zipWithIndex

  // Фильтруем четные и нечетные индексы
  val evenIndex = indexNum.filter { case (_, index) => index % 2 == 0 }.map(_._1)
  val oddIndex = indexNum.filter { case (_, index) => index % 2 != 0 }.map(_._1)

  (evenIndex, oddIndex)
}

def printfindMax(nums: List[Int]) = {
  println("Максимальное число " + nums.reduce((a, b) => if (a > b) a else b) + ". В наборе " + nums)
}
// 1.3

// 1.5
def describePoint(point: (Int, Int)): Unit = {
  point match {
    case (0, 0) => println("В начале координат")
    case (x, 0) => println(s"На оси X, x = $x")
    case (0, y) => println(s"На оси Y, y = $y")
    case (x, y) => println(s"Точка в координатах ($x, $y)")
  }
}
// 1.5

// 1.6
def compose(a:Int=>Double, b:Double=>String):Int=>String = x=>b(a(x))

val funcOne:Int=>Double = x=>1.5*x
val funcTwo:Double=>String=x=>if(x>10)"Yes"else"No"
//1.6