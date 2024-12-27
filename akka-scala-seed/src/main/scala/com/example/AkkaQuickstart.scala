package com.example

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.example.GreeterMain.SayHello

// Объект, определяющий поведение актора Greeter
object Greeter {
  // Сообщения, которые может обрабатывать Greeter
  //    Greet - сообщение с именем и адресом актора для ответа
  final case class Greet(whom: String, replyTo: ActorRef[Greeted])
  //    Greeted - сообщение-ответ с именем и адресом актора, отправившего Greet
  final case class Greeted(whom: String, from: ActorRef[Greet])

  // Определение поведения актора Greeter
  def apply(): Behavior[Greet] = Behaviors.receive { (context, message) =>
    // Логирование полученного приветствия
    context.log.info("Hello {}!", message.whom)
    // Отправка ответа актору, указанному в replyTo
    message.replyTo ! Greeted(message.whom, context.self)
    // Актора остается в текущем состоянии
    Behaviors.same
  }
}

// Объект, определяющий поведение актора GreeterBot
object GreeterBot {
  // Создание актора GreeterBot с максимальным количеством приветствий
  def apply(max: Int): Behavior[Greeter.Greeted] = {
    bot(0, max) // Вызов внутреннего метода bot для инициализации поведения
  }

  // Внутренний метод, реализующий поведение GreeterBot
  private def bot(greetingCounter: Int, max: Int): Behavior[Greeter.Greeted] =
    Behaviors.receive { (context, message) =>
      // Увеличение счетчика приветствий
      val n = greetingCounter + 1
      // Логирование текущего приветствия
      context.log.info("Greeting {} for {}", n, message.whom)
      // Проверка, не превышено ли максимальное количество приветствий
      if (n == max) {
        // Если максимальное количество достигнуто - остановка актора
        Behaviors.stopped
      } else {
        // Отправка нового приветствия актору, от которого пришло сообщение
        message.from ! Greeter.Greet(message.whom, context.self)
        // Переход в следующее состояние актора (с обновленным счетчиком)
        bot(n, max)
      }
    }
}

// Объект, определяющий поведение актора GreeterMain
object GreeterMain {
  // Сообщение, которое может обрабатывать GreeterMain
  final case class SayHello(name: String)

  // Определение поведения актора GreeterMain
  def apply(): Behavior[SayHello] =
    Behaviors.setup { context =>
      // Создание дочернего актора Greeter
      val greeter = context.spawn(Greeter(), "greeter")

      // Определение поведения для сообщений SayHello
      Behaviors.receiveMessage { message =>
        // Создание дочернего актора GreeterBot (по одному на каждое имя)
        val replyTo = context.spawn(GreeterBot(max = 3), message.name)
        // Отправка приветствия на актора Greeter, с адресом актора GreeterBot для ответа
        greeter ! Greeter.Greet(message.name, replyTo)
        // Актора остается в текущем состоянии
        Behaviors.same
      }
    }
}

// Основная программа, запускающая ActorSystem
object AkkaQuickstart extends App {
  // Создание ActorSystem с поведением GreeterMain
  val greeterMain: ActorSystem[GreeterMain.SayHello] = ActorSystem(GreeterMain(), "AkkaQuickStart")
  // Отправка сообщения SayHello с именем "Charles"
  greeterMain ! SayHello("Charles")
}