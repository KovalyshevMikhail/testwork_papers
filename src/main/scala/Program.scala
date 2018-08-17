import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import market.Market

object Program {
  /**
    * Список аргументов программы, с указанием, кого точно нужно заполнить
    *
    */
  private val argsMustBe = Map(
    "clients" -> ProgramArgument(name = "clients", description = "Путь до файла со списком клиентов и их балансом", example = "-clients C:\\market\\clients.txt", critical = true),
    "orders" -> ProgramArgument(name = "orders", description = "Путь до файла со списком ордеров биржи в хронологическом порядке", example = "-orders C:\\market\\orders.txt", critical = true),
    "result" -> ProgramArgument(name = "result", description = "Путь до результирующего файла со списком клиентов и их конечным балансом", example = "-result C:\\market\\result.txt", critical = true),
    "help" -> ProgramArgument(name = "help", description = "Вывод справки по использованию", example = "", critical = false)
  )

  /**
    * Метод генерирует последовательность из переданных символов в переданном количестве
    *
    * @param count     количество
    * @param separator символ(ы)
    * @return результирующая строка
    */
  def genStr(count: Int, separator: String): String =
    Range(0, count).foldLeft(separator)((r, _) => r + separator)

  /**
    * Метод выводит простую справку по использованию
    *
    */
  def showHelp(): Unit = {
    println("\nСправка по использованию программы:\n")
    println(f"| ${"Аргумент"}%-10s | ${"Описание"}%-75s | ${"Пример использования"}%-30s |")
    println(s"| ${genStr(10, "-")}| ${genStr(75, "-")}| ${genStr(30, "-")}|")
    argsMustBe.foreach(arg => {
      println(f"| -${arg._2.name}%-9s | ${arg._2.description}%-75s | ${arg._2.example}%-30s |")
    })
    println("\n")
  }

  /**
    * Метод перебирает переданные аргументы и загружает их
    *
    * @param args общий список аргументов
    * @return результат, что все хорошо загрузилось
    */
  def parseArgs(args: Array[String]): Boolean = {
    for (iter <- args.indices) {
      val searchName = args(iter).replace("-", "")
      val argument = argsMustBe.find(p => p._1 == searchName)
      if (argument.isDefined) {
        if (args.length > iter + 1) {
          argument.get._2.value = args(iter + 1)

        }
      }
    }

    !argsMustBe.exists(p => p._2.value.isEmpty && p._2.critical)
  }

  /**
    * Запускатор
    *
    * @param args аргументы
    */
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      showHelp()
    }
    else {
      println("Считываем аргументы")
      // 1 парсим аргументы
      if (parseArgs(args)) {
        println("Все обязательные аргументы считаны.")
        if (Market.initializeClients(Paths.get(argsMustBe("clients").value).toFile)) {
          println("Клиенты проинициализированы:")
          println(Market.dumpClients())
          println("Производин расчеты")
          Market.computeOrders(Paths.get(argsMustBe("orders").value).toFile)
          println("\nКлиенты после расчетов:")

          val fos = new FileOutputStream(
            new File(argsMustBe("result").value)
          )
          try {
            val result = Market.dumpClients()
            println(result)
            fos.write(result.getBytes(StandardCharsets.UTF_8))
          }
          catch {
            case ex: Exception => {
              println(s"При записи в файл [${argsMustBe("result").value}] возникли ошибки: \n${ex.getMessage}")
              ex.printStackTrace()
            }
          }
          finally {
            try {
              fos.close()
            }
            catch {
              case ex: Exception => {
                println(s"При закрытии файла [${argsMustBe("result").value}] возникли ошибки: \n${ex.getMessage}")
                ex.printStackTrace()
              }
            }
          }
        }
        else {
          println("Во время инициализации клиентов произошла ошибка")
        }
      }
      else {
        println("Не все обязательные аргументы заполнены. Проверьте, пожалуйста:")
        argsMustBe.filter(p => p._2.value.isEmpty && p._2.critical).foreach(p => println(s" - ${p._1};"))
        showHelp()
      }

    }
  }
}

/**
  * Класс-описание аргумента программы
  *
  * @param name        название
  * @param value       значение (заполняется потом)
  * @param description описание (для вывода в справку)
  * @param example     пример использования (для вывода в справку)
  * @param critical    критичность (значит, что без этого аргумента программа не будет работать)
  */
case class ProgramArgument(name: String, var value: String = "", description: String, example: String, critical: Boolean)