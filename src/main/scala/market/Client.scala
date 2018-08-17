package market

import scala.collection.mutable

/**
  * Класс хранения информации о клиенте
  *
  * @param name          имя клиента
  * @param moneyBalance  баланс денег
  * @param paperBalances баланс по ценным бумагам
  */
case class Client(name: String, var moneyBalance: Double, paperBalances: mutable.Map[String, Int]) {
  override def toString: String =
    f"$name\t${moneyBalance.toInt}%d${paperBalances.foldLeft("")((acc, paper) => s"$acc\t${paper._2}")}"
}

/**
  * Объект для создания новой клиента по переданной отформатированной строке
  */
object Client {
  // регулярка по распарсиванию
  private val parseLine = "^(\\w+)\\s(\\d+)\\s(\\d+)\\s(\\d+)\\s(\\d+)\\s(\\d+)$".r
  // количество ценных бумаг (может можно как-то по-другому, скорее всего так и есть...)
  private val countPapers = 4

  /**
    * Метод генерирует букву для названия ценной бумаги
    *
    * @param iter номер буквы по счету от 1
    * @return буква
    */
  def getSimplePaperName(iter: Int): String = {
    Range(0, iter).foldLeft('A')((acc, _) => (acc.toInt + 1).toChar).toString
  }

  /**
    * Метод по переданной строке создает нового клиента
    *
    * @param line отформатированная строка с табуляциями
    * @return объект клиента или null
    */
  def newClientFromLine(line: String): Client = {
    if (line != null) {
      val found = parseLine.findFirstMatchIn(line)
      if (found.isDefined) {
        Client(
          found.get.group(1),
          found.get.group(2).toDouble,
          Range(0, countPapers).foldLeft(mutable.Map[String, Int]())((acc, element) => {
            acc + (getSimplePaperName(element) -> found.get.group(element + 3).toInt)
          })
        )
      }
      else {
        null
      }
    }
    else {
      null
    }
  }
}
