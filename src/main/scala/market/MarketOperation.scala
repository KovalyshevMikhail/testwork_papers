package market

/**
  * Класс хранения Заявки / Операции на бирже
  *
  * @param clientName    клиент, от которого пришла заявка
  * @param operationType тип заявки (продажа / покупка)
  * @param paperName     название ценной бумаги
  * @param priceOfOne    цена одной штуки
  * @param counts        количество штук
  */
case class MarketOperation(clientName: String, operationType: String, paperName: String, priceOfOne: Int, counts: Int) {
  // Буду использовать для одзначной идентификации заявки
  override def hashCode(): Int = (paperName, priceOfOne, counts).##
}

/**
  * Объект для создания новой заявки по переданной отформатированной строке
  *
  */
object MarketOperation {
  // по регулярке парсим
  private val parseLine = "^(\\w+)\\s(\\w+)\\s(\\w+)\\s(\\d+)\\s(\\d+)".r

  /**
    * Метод создает новую заявку по переданной строке
    *
    * @param line строка с заявкой
    * @return либо объект, либо null
    */
  def newInstance(line: String): MarketOperation = {
    if (line != null) {
      val found = parseLine.findFirstMatchIn(line)
      if (found.isDefined) {
        MarketOperation(
          found.get.group(1),
          found.get.group(2),
          found.get.group(3),
          found.get.group(4).toInt,
          found.get.group(5).toInt
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
