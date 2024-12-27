import scala.util.{Try, Success, Failure} 
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.io.StdIn
import scala.util.matching.Regex

//-----------------------------------------------
// 1

// Функция интеграла, f - функция, l - левая граница, r - правая граница, steps - количество интервалов
def integral(f: Double => Double, l: Double, r: Double, steps: Int): Double = {
val stepSize = (r - l) / steps
(0 until steps).map(i => l + i * stepSize).map(x => f(x) * stepSize).sum
}

@main def main(): Unit = {
// Пример использования:
val func: Double => Double = x => x * x
val leftBound = 0.0
val rightBound = 1.0
val numSteps = 1000
println(s"Integral of x^2 from $leftBound to $rightBound with $numSteps steps: ${integral(func, leftBound, rightBound, numSteps)}")

// Пример с другой функцией и границами
val sinFunc: Double => Double = x => math.sin(x)
val leftBoundSin = 0.0
val rightBoundSin = math.Pi
val numStepsSin = 10000
println(s"Integral of sin(x) from $leftBoundSin to $rightBoundSin with $numStepsSin steps: ${integral(sinFunc, leftBoundSin, rightBoundSin, numStepsSin)}")
}

//-----------------------------------------------
// 2.1

// Проверка на длинну, нахождение символа заглавного и маленького, цифру и спиальный знак в пароле
def goodEnoughPassword1(password: String): Option[String] =
    List(
        Option.when(password.length < 8)("Password must be at least 8 characters long."),
        Option.when(!"[A-Z]".r.findFirstIn(password).isDefined)("Password must contain at least one uppercase letter."),
        Option.when(!"[a-z]".r.findFirstIn(password).isDefined)("Password must contain at least one lowercase letter."),
        Option.when(!"[0-9]".r.findFirstIn(password).isDefined)("Password must contain at least one digit."),
        Option.when(!"[^a-zA-Z0-9\\s]".r.findFirstIn(password).isDefined)("Password must contain at least one special character.")
    ).collectFirst { case Some(msg) => msg }


@main def main2(): Unit = {
    println(goodEnoughPassword1("short"))                  // Some("Password must be at least 8 characters long.")
    println(goodEnoughPassword1("onlylowercase"))         // Some("Password must contain at least one uppercase letter.")
    println(goodEnoughPassword1("ONLYUPPERCASE"))         // Some("Password must contain at least one lowercase letter.")
    println(goodEnoughPassword1("NoSpecialChars1"))       // Some("Password must contain at least one special character.")
    println(goodEnoughPassword1("NoDigitsSpecialChar"))   // Some("Password must contain at least one digit.")
    println(goodEnoughPassword1("GoodP@ss1"))            // None
}

//-----------------------------------------------
// 2.2

def goodEnoughPassword2(password: String): Either[Boolean, String] =
        Try {
            List(
                Either.cond(password.length >= 8, (), "Password must be at least 8 characters long.").map(_ => ()),
                Either.cond("[A-Z]".r.findFirstIn(password).isDefined, (), "Password must contain at least one uppercase letter.").map(_ => ()),
                Either.cond("[a-z]".r.findFirstIn(password).isDefined, (), "Password must contain at least one lowercase letter.").map(_ => ()),
                Either.cond("[0-9]".r.findFirstIn(password).isDefined, (), "Password must contain at least one digit.").map(_ => ()),
                Either.cond("[^a-zA-Z0-9\\s]".r.findFirstIn(password).isDefined, (), "Password must contain at least one special character.").map(_ => ())
            ).collectFirst {
               case Left(msg) => msg
            } match {
                case Some(msg) => Right(msg)
                case None => Left(true)
            }
        } match {
            case Success(result) => result
            case Failure(_) => Left(false)
        }
    
@main def main3(): Unit = {
    println(goodEnoughPassword2("short")) //Right(Password must be at least 8 characters long.)
    println(goodEnoughPassword2("onlylowercase")) //Right(Password must contain at least one uppercase letter.)
    println(goodEnoughPassword2("ONLYUPPERCASE")) //Right(Password must contain at least one lowercase letter.)
    println(goodEnoughPassword2("NoSpecialChars1")) //Right(Password must contain at least one special character.)
    println(goodEnoughPassword2("NoDigitsSpecialChar")) //Right(Password must contain at least one digit.)
    println(goodEnoughPassword2("GoodP@ss1")) // Left(true)
    println(goodEnoughPassword2(null)) // Left(false)
}

//-----------------------------------------------
// 2.3

// Проверка введенного пароля на плавильность с помощью функции их пункта 2.2
def readPassword(): Future[String] = {
    def readAndValidate(): Future[String] = {
        Future(StdIn.readLine("Enter password:")).flatMap { input =>
            goodEnoughPassword2(input) match {
                case Left(true) => Future.successful(input)
                case Right(error) => println(s"Password is not good enough: $error. Please try again!"); readAndValidate()
                case Left(false) => println("An unknown error occurred. Please try again!"); readAndValidate()
            }
        }
    }
    readAndValidate()
}

@main def main4(): Unit = {
    val passwordFuture: Future[String] = readPassword()
    val result = Await.result(passwordFuture, Duration.Inf)
    println(s"Your password is: $result")
}

//-----------------------------------------------
// 3

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F]{
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}
