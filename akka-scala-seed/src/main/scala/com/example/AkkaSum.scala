import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import scala.util.Random
import scala.math.BigDecimal

// Определение типов данных для операций
sealed trait Datum
case class IntegerDatum(value: Int) extends Datum
case class DecimalDatum(value: BigDecimal) extends Datum
case class TextDatum(value: String) extends Datum

// Актор для выполнения агрегации данных
object DataAggregator {
  // Сообщение: данные для агрегации и ссылка на актор для ответа
    case class AggregateData(x: Datum, y: Datum, respondTo: ActorRef[Datum])

    // Определение поведения актора
    def apply(): Behavior[AggregateData] = Behaviors.receive{ (context, message) =>
        val outcome = (message.x, message.y) match {
            case (TextDatum(x), y: Datum)        => TextDatum(x + y.toString)
            case (x: Datum, TextDatum(y))        => TextDatum(y + x.toString)
            case (IntegerDatum(x), IntegerDatum(y)) => IntegerDatum(x + y)
            case (DecimalDatum(x), DecimalDatum(y)) => DecimalDatum(x + y)
            case (IntegerDatum(_), DecimalDatum(_))    => ???
            case (DecimalDatum(_), IntegerDatum(_))    => ???
        }
        message.respondTo ! outcome
        Behaviors.same
    }
}

// Актор для генерации и отправки запросов на агрегацию данных
object DataSource {
    import DataAggregator.AggregateData
    
     // Определение поведения актора
    def apply(aggregator: ActorRef[AggregateData]): Behavior[Datum] = Behaviors.setup { context =>
        val generator = new Random()

        aggregator ! AggregateData(
            IntegerDatum(generator.nextInt(200)),
            IntegerDatum(generator.nextInt(200)),
            context.self
        )
        aggregator ! AggregateData(
           DecimalDatum(BigDecimal(generator.nextDouble() * 100)),
           DecimalDatum(BigDecimal(generator.nextDouble() * 100)),
           context.self
        )
        aggregator ! AggregateData(
         TextDatum("Value is: "),
         TextDatum(generator.nextInt(200).toString),
         context.self
      )
      // Обработка ответа и вывод
        Behaviors.receiveMessage { result =>
            context.log.info(s"Data outcome: $result")
            Behaviors.same
        }
    }
}

// Менеджер системы обработки данных
object DataSystem {
  import DataAggregator.AggregateData
  
  // Определение поведения системы
  def apply(): Behavior[Unit] = Behaviors.setup { context =>
     // Создание единственного актора агрегатора
    val aggregator = context.spawn(DataAggregator(), "aggregator")
    
    // Создание актора источника данных и отправка сообщений агрегатору
    context.spawn(DataSource(aggregator), "dataSource")

    Behaviors.empty
  }
}

// Точка входа в систему
@main def MainApp(): Unit = {
  // Запуск системы
  ActorSystem(DataSystem(), "data-system")
}