package module3

import zio.{Has, RIO, Task, UIO, ULayer, URIO, ZIO, ZLayer, clock, console, random}
import zio.clock._
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps
import zioConcurrency.printEffectRunningTime

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  val eff1: URIO[Random, Int] = random.nextIntBetween(1, 4)
  val eff2: ZIO[Console, IOException, Int] = console.putStrLn("Угадай число от 1 до 3..") *> console.getStrLn.map(_.toInt)
  def eff3(i1: Int, i2: Int): URIO[Console, Unit] = console.putStrLn( if(i1==i2) "Угадал" else "Не угадал")

  lazy val guessProgram1: ZIO[Console with Random, IOException, Unit] = for{
    e1 <- eff1
    e2 <- eff2
    _ <- eff3(e1,e2)
  } yield ()

  lazy val guessProgram3: ZIO[Console with Random, IOException, Boolean] = for{
    e1 <- eff1
    e2 <- eff2
    _ <- eff3(e1,e2)
  } yield (e1==e2)


  lazy val guessProgram2: ZIO[Random with Console, IOException, Unit] = for {
    randomizer <- ZIO.environment[Random].map(_.get)
    console <- ZIO.environment[Console].map(_.get)

    key <- randomizer.nextIntBetween(1, 4)
    _ <- console.putStrLn("Угадай число от 1 до 3..")
    i <- console.getStrLn.map(_.toInt)
    _ <- console.putStrLn( if( i == key) "угадал" else "не угадал" )
  } yield ()


  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R,E]( a: ZIO[R, E, Boolean]): ZIO[R,E,Any] =
    a.flatMap( (key) => if(!key) doWhile(a) else ZIO.unit)

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault = config.load.orElse( ZIO.succeed(config.AppConfig("","")) )

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: URIO[Random with Clock, Int] = for {
    _ <- ZIO.sleep(1 seconds)
    random <- ZIO.environment[Random].map(_.get)
    int <- random.nextIntBetween(0,10)
  } yield int

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = List.fill(10)(eff)

  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: URIO[Clock with Console with Random, Int] = printEffectRunningTime( ZIO.reduceAll(ZIO.succeed(0), effects){ (acc, cur) => acc+cur } )

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp: URIO[Clock with Console with Random, Int] = printEffectRunningTime( ZIO.reduceAllPar(ZIO.succeed(0), effects){ (acc, cur) => acc+cur } )

  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  object printEffectRunningTimeService1 {
    type PrintEffectRunningTimeService1 = Has[PrintEffectRunningTimeService1.Service]

    object PrintEffectRunningTimeService1{

      trait Service{
        def print[R, E, A](a: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A]
      }

      class ServiceImpl extends Service{
        override def print[R, E, A](a: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A] = printEffectRunningTime(a)
      }

      val live =
        ZLayer.fromService[Clock with Console, PrintEffectRunningTimeService1.Service](_ => new ServiceImpl())
    }
  }

  object printEffectRunningTimeService2 {
    trait PrintEffectRunningTimeService2 {
      def print[R, E, A](a: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A]
    }

    val printEffectRunningTimeService2Impl: PrintEffectRunningTimeService2 = new PrintEffectRunningTimeService2{
      override def print[R, E, A](a: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A] = printEffectRunningTime(a)
    }
  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  import printEffectRunningTimeService2._
  lazy val appWithTimeLogg: ZIO[Random with Clock with Console with PrintEffectRunningTimeService2, Nothing, Unit] = for {
    logger <- ZIO.environment[PrintEffectRunningTimeService2]
    _ <- logger.print(ZIO.reduceAll(ZIO.succeed(0), effects){ (acc, cur) => acc+cur })
  } yield ()

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  /** к сожалению так нельзя, не компилируется */
  //lazy val runApp2: ZIO[Clock with Console with Random, Nothing, Unit] = appWithTimeLogg.provideSome[Random with Clock with Console]( printEffectRunningTimeService2Impl)

  /** абсолютно все гуглимые примеры как будто не запускались ни разу, так как нет у env никаких имплементаций, это раз,..
   * а второе, я не могу создать new Clock with Console with Random, потому что illegal inheritance from final class Has
   * казалось бы даже в описании метода provideSome указан пример, но он не компилится... */

  /*lazy val runApp2: ZIO[Any, Nothing, Unit] = appWithTimeLogg.provideSome[Console]( env =>
    new Clock with Console with Random with PrintEffectRunningTimeService2 {
       val clock = env.clock
       val console = env.console
       val random = env.random
       val printEffectRunningTimeService2 = printEffectRunningTimeService2Impl
    })*/

  /*lazy val runApp2: ZIO[Clock with Random, Nothing, Unit] = appWithTimeLogg.provideSome[Clock with Random]( env =>
    new Console with PrintEffectRunningTimeService2 {
       val console = env.console
       val printEffectRunningTimeService2 = printEffectRunningTimeService2Impl
    })*/

  /** Вот так оказывается тоже нельзя, видимо потому что у printEffect тоже есть зависимости, но как это все скомбинировать - не понимаю */
  //lazy val runApp1: ZIO[Clock with Console with Random, Nothing, Unit] = appWithTimeLogg.provideSomeLayer[Clock with Console with Random](printEffectRunningTimeService1.PrintEffectRunningTimeService1.live)

  /** И даже так нельзя, в общем все, я перестал что либо понимать. */
  //lazy val runApp1: ZIO[Any, Nothing, Unit] = appWithTimeLogg.provideLayer(Clock.live ++ Console.live ++ Random.live ++ printEffectRunningTimeService1.PrintEffectRunningTimeService1.live)

  /** На лекциях выглядело что все очень просто, в реальности все горяздо хуже, помогите, объясните что я не так понимаю */
}
