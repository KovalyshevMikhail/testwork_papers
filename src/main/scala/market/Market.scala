package market

import java.io.File
import java.util.concurrent.LinkedBlockingQueue

import scala.collection.mutable
import scala.io.Source

/**
  * Объект биржи.
  * В объекте содержится информация о подключенных клиентах и их балансе.
  *
  */
object Market {
  /**
    * Метод замеряет время выполнения переданного блока операций
    *
    * @param block блок операций
    * @tparam R возвращаемый тип результата
    * @return результат
    */
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Потрачено: " + (t1 - t0) / 1000000 + "мс")
    result
  }

  /**
    * Хранилище клиентов
    */
  private val clients = mutable.LinkedHashMap[String, Client]()

  /**
    * Хранилище заявок на покупку
    */
  private val stackBuy = mutable.HashMap[Int, LinkedBlockingQueue[MarketOperation]]()
  // идентификатор заявки на покупку
  private val BUY_OPERATION = "b"

  /**
    * Хранилище заявок на продажу
    */
  private val stackSale = mutable.HashMap[Int, LinkedBlockingQueue[MarketOperation]]()
  // идентификатор заявки на продажу
  private val SALE_OPERATION = "s"

  /**
    * Метод загружает клиентов из переаданного файла
    *
    * @param pathToClients файл
    * @return результат выполнения операции (успешно или нет)
    */
  def initializeClients(pathToClients: File): Boolean = {
    if (pathToClients.canRead) {
      try {
        val reader = Source.fromFile(pathToClients).bufferedReader()
        reader
          .lines()
          .forEach(createClient)

        reader.close()

        clients.nonEmpty
      }
      catch {
        case ex: Exception => {
          println(s"Не удалось считать клиентов из файла [${pathToClients.getName}]: ${ex.getMessage}")
          ex.printStackTrace()

          false
        }
      }
    }
    else {
      println(s"Невозможно прочитать файл [${pathToClients.getName}]")
      false
    }
  }

  /**
    * Метод создает и добавляет нового клиента по переданной строке
    *
    * @param line отформатированная строка
    * @return результат выполнения операции (успешно или нет)
    */
  def createClient(line: String): Boolean = {
    try {
      val created = Client.newClientFromLine(line)
      if (created != null) {
        clients += (created.name -> created)

        true
      }
      else {
        false
      }
    }
    catch {
      case ex: Exception => {
        println(s"По переданной строке [$line] не удалось создать клиента: ${ex.getMessage}")
        ex.printStackTrace()
        false
      }
    }
  }

  /**
    * По переданному файлу производится выборка и расчет операций
    *
    * @param pathToOrders файл
    * @return результат выполнения операции (успешно или нет)
    */
  def computeOrders(pathToOrders: File): Boolean = {
    time {
      if (pathToOrders.canRead) {
        try {
          Source
            .fromFile(pathToOrders)
            .bufferedReader()
            .lines()
            .forEach(
              line => computeOrder(MarketOperation.newInstance(line))
            )

          true
        }
        catch {
          case ex: Exception => {
            println(s"Возникла ошибка при работе с файлом [${pathToOrders.getName}]: ${ex.getMessage}")
            ex.printStackTrace()

            false
          }
        }
      }
      else {
        // println(s"Переданный файл [${pathToOrders.getName}] не может быть прочитан.")
        false
      }
    }
  }

  /**
    * Метод рассчитывает переданную заявку в зависимости от типа
    *
    * @param order заявка
    * @return результат выполнения операции (успешно или нет)
    */
  def computeOrder(order: MarketOperation): Boolean = {
    if (order != null) {
      order.operationType match {
        case BUY_OPERATION => {
          computeOperation(order, stackBuy, stackSale)
        }
        case SALE_OPERATION => {
          computeOperation(order, stackSale, stackBuy)
        }
        case _ => {
          // println(s"Незнакомая операция: [$order]")
        }
      }
      true
    }
    else {
      false
    }
  }

  /**
    * Метод по переданной операции и хранилищам ищет подходящую операцию.
    * В случае, если операция найдена, - то операция выполняется.
    * Иначе - операция сохраняется в хранилище.
    *
    * @param operation   - операция
    * @param sourceStack - хранилище, откуда заявка пришла
    * @param targetStack - хранилище, где заявка должна быть найдена
    * @return результат выполнения операции (успешно или нет)
    */
  private def computeOperation(
                                operation: MarketOperation,
                                sourceStack: mutable.HashMap[Int, LinkedBlockingQueue[MarketOperation]],
                                targetStack: mutable.HashMap[Int, LinkedBlockingQueue[MarketOperation]]
                              ): Boolean = {
    if (targetStack.contains(operation.hashCode()) && targetStack(operation.hashCode()).size() > 0) {
      val fromOperation = targetStack(operation.hashCode()).poll()
      if (completeOperation(fromOperation, operation)) {
        if (targetStack(operation.hashCode()).isEmpty) {
          targetStack.remove(operation.hashCode())
        }
        //println("Операция произведена успешно")
        true
      }
      else {
        //println("Во время проведения операции произошли ошибки")
        false
      }
    }
    else {
      // если не нашли операцию в продажах - добавляем к себе
      if (!sourceStack.contains(operation.hashCode())) {
        sourceStack += (operation.hashCode() -> new LinkedBlockingQueue[MarketOperation]())
      }
      sourceStack(operation.hashCode()).put(operation)
      //println("Операция по данным параметрам не найдена. Добавляем в список ожидания")
      true
    }
  }

  /**
    * Метод обновляет баланс клиентов по переданным операциям
    *
    * @param from операция от кого
    * @param to   операция кому
    * @return результат выполнения операции (успешно или нет)
    */
  private def completeOperation(from: MarketOperation, to: MarketOperation): Boolean = {
    /**
      * Метод по обновлению баланса в зависимости от операции
      *
      * @param client    клиент, для которого нужно обновить баланс
      * @param operation операция, по которой баланс будет обновляться
      * @return результат выполнения операции (успешно или нет)
      */
    def updateBalance(client: Client, operation: MarketOperation): Boolean = {
      try {
        operation.operationType match {
          case BUY_OPERATION => {
            client.moneyBalance -= (operation.priceOfOne * operation.counts).toDouble
            client.paperBalances(operation.paperName) += operation.counts
          }
          case SALE_OPERATION => {
            client.moneyBalance += (operation.priceOfOne * operation.counts).toDouble
            client.paperBalances(operation.paperName) -= operation.counts
          }
          case _ =>
            println(s"Незнакомая операция: [$operation]")

        }

        true
      }
      catch {
        case ex: Exception => {
          println(s"Ошибка при обновлении баланса. \nКлиент [$client]\nОперация [$operation]. \n ${ex.getMessage}")
          ex.printStackTrace()
          false
        }
      }
    }

    if (from != null && to != null) {
      if (updateBalance(clients(from.clientName), from) && updateBalance(clients(to.clientName), to)) {
        true
      }
      else {
        false
      }
    }
    else {
      false
    }

  }

  /**
    * Метод выводит список клиентов в строку
    */
  def dumpClients(): String =
    clients.foldLeft(new StringBuilder())((acc, client) => {
      acc.append(s"${client._2}\n")
    }).toString()

  /**
    * Метод выводит состояние очереди покупки в строку
    */
  def dumpStackBuy(): String =
    stackBuy.foldLeft(new StringBuilder())((acc, paper) => {
      acc.append(s"$paper\n")
    }).toString()

  /**
    * Метод выводит состояние очереди продажи в строку
    */
  def dumpStackCell(): String =
    stackSale.foldLeft(new StringBuilder())((acc, paper) => {
      acc.append(s"$paper\n")
    }).toString()
}

