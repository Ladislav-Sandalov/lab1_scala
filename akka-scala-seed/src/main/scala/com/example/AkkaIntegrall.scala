import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

// Объект, отвечающий за вычисление интеграла на заданном интервале
object IntegralRangeCalculator {
  //  Сообщение для вычисления интеграла
  case class CalculateRange(
      function: Double => Double, // Функция для вычисления
      start: Double, // Начало интервала
      stepSize: Double, // Размер шага
      totalSteps: Int, // Количество шагов
      replyTo: ActorRef[RangeCalculationResult] // Актор, которому нужно отправить результат
  )
    //  Сообщение с результатом вычисления интеграла
  case class RangeCalculationResult(partialSum: Double) // Результат вычисления

  //  Определение поведения актора
  def apply(): Behavior[CalculateRange] = Behaviors.receive {
    (context, message) =>
      //  Вычисление интеграла
      val totalSum = (0 until message.totalSteps)
        .map(i => message.start + i * message.stepSize)
        .map(x => message.function(x) * message.stepSize)
        .sum

      //  Отправка результата
      message.replyTo ! RangeCalculationResult(totalSum)
      //  Актора остается в том же состоянии
      Behaviors.same
  }
}

// Объект, отвечающий за агрегацию частичных результатов
object PartialSumAggregator {
   //  Сообщение с частичным результатом
  case class AddPartialSum(partialSum: Double) // Частичный результат

    //  Определение поведения актора
  def apply(totalParts: Int, replyTo: ActorRef[Double]): Behavior[AddPartialSum] =
    Behaviors.setup { context =>
      //  Вспомогательная функция для агрегации результатов
      def aggregate(
          currentSum: Double, // Текущая сумма
          remainingParts: Int // Оставшееся количество частей
      ): Behavior[AddPartialSum] = {
          //  Обработка сообщения с частичным результатом
        Behaviors.receiveMessage { case AddPartialSum(partialSum) =>
          //  Обновление текущей суммы
          val updatedSum = currentSum + partialSum
          //  Логгирование полученного частичного результата
          context.log.info(
            s"Received partial sum: $partialSum, remaining parts: ${remainingParts - 1}"
          )
          //  Проверка, все ли части получены
          if (remainingParts - 1 == 0) {
            //  Если все части получены - отправка результата
            replyTo ! updatedSum
            //  Остановка актора
            Behaviors.stopped
          } else {
            //  Продолжение агрегации
            aggregate(updatedSum, remainingParts - 1)
          }
        }
      }
    //  Начало агрегации
      aggregate(0.0, totalParts)
    }
}

// Объект, отвечающий за логирование финального результата
object FinalResultLogger {
  //  Определение поведения актора
  def apply(): Behavior[Double] = Behaviors.receive { (context, result) =>
      //  Вывод финального результата в лог
    context.log.info(s"Final calculated sum: $result")
      //  Актора остается в том же состоянии
    Behaviors.same
  }
}

// Объект, управляющий процессом расчета
object CalculationController {
    //  Сообщение для начала вычисления интеграла
  case class StartCalculation(
      function: Double => Double, // Функция для вычисления
      lowerBound: Double, // Нижняя граница интегрирования
      upperBound: Double, // Верхняя граница интегрирования
      steps: Int // Количество шагов
  )
    //  Определение поведения актора
  def apply(): Behavior[StartCalculation] = Behaviors.setup { context =>
      //  Создание актора для вычисления интеграла на интервале
    val integralCalculator =
      context.spawn(IntegralRangeCalculator(), "integralRangeCalculator")

    Behaviors.receiveMessage {
        //  Обработка сообщения начала вычисления
      case StartCalculation(function, lowerBound, upperBound, steps) =>
        //  Вычисление размера шага
        val stepSize = (upperBound - lowerBound) / steps
        //  Количество акторов для параллельного вычисления
        val numberOfActors = 4

       //  Создание актора для логирования результатов
        val resultLogger = context.spawn(FinalResultLogger(), "finalResultLogger")
        // Создание актора для агрегации частичных результатов
        val aggregator = context.spawn(
          PartialSumAggregator(numberOfActors, resultLogger),
          "partialSumAggregator"
        )
        //  Функция для инициации вычислений
        def initiateCalculation(
            start: Double, // Начало интервала
            steps: Int, // Количество шагов
            replyTo: ActorRef[IntegralRangeCalculator.RangeCalculationResult] // Актор для получения результата
        ): Unit =
            // 30. Отправка сообщения для вычисления интеграла на интервале
          integralCalculator ! IntegralRangeCalculator.CalculateRange(
            function,
            start,
            stepSize,
            steps,
            replyTo
          )
        //  Запуск вычислений на нескольких акторах
        (0 until numberOfActors).foreach { i =>
          //  Вычисление начала интервала для каждого актора
          val start = lowerBound + i * (steps / numberOfActors) * stepSize
           //  Создание актора для обработки ответа
          val responseActor = context.spawn(
            Behaviors.receiveMessage[IntegralRangeCalculator.RangeCalculationResult] {
                //  Обработка полученного частичного результата
              case IntegralRangeCalculator.RangeCalculationResult(partialSum) =>
                  //  Отправка частичного результата на агрегатор
                aggregator ! PartialSumAggregator.AddPartialSum(partialSum)
                // Актора остается в том же состоянии
                Behaviors.same
            },
            s"responseActor-$i"
          )
        // Запуск вычисления на данном интервале
          initiateCalculation(start, steps / numberOfActors, responseActor)
        }

        // Актора остается в том же состоянии
        Behaviors.same
    }
  }
}

// Главная функция для запуска системы
@main def runIntegralCalculation(): Unit = {
    // Создание ActorSystem
  val system =
    ActorSystem(CalculationController(), "integralCalculationSystem")
    // Отправка сообщения для начала вычисления интеграла
  system ! CalculationController.StartCalculation(
    x => math.sqrt(x * x * x + 10),
    0,
    25,
    250
  )
}