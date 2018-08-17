import market.{Market, MarketOperation}
import org.scalatest._

class OkTest extends FunSuite{
  test("После старта программы список клиентов, очереди покупок и продаж пустые") {
    assert(Market.dumpClients().isEmpty)
    assert(Market.dumpStackBuy().isEmpty)
    assert(Market.dumpStackCell().isEmpty)
  }

  test("Добавили клиента TEST1 - появилась одна строка") {
    assert(Market.createClient("TEST1\t1000\t0\t1\t0\t1"))
    assert(!Market.dumpClients().isEmpty)
  }

  test("Создаем клиента TEST2, добавляем заявку на продажу и покупку для каждого по 2ой бумаге") {
    assert(Market.createClient("TEST2\t1000\t1\t0\t1\t0"))
    val operationTest1_to_Test2_cell = MarketOperation.newInstance("TEST1\ts\tB\t100\t1")
    val operationTest2_to_Test1_buy = MarketOperation.newInstance("TEST2\tb\tB\t100\t1")
    assert(Market.computeOrder(operationTest1_to_Test2_cell))
    assert(Market.computeOrder(operationTest2_to_Test1_buy))
  }

  test("После сделки очереди покупки и продаж должны быть пустые") {
    assert(Market.dumpStackBuy().isEmpty)
    assert(Market.dumpStackCell().isEmpty)
  }

  test("Добавляем заявку на продажу и покупку для каждого по 4ой бумаге") {
    val operationTest1_to_Test2_cell = MarketOperation.newInstance("TEST1\ts\tD\t100\t1")
    val operationTest2_to_Test1_buy = MarketOperation.newInstance("TEST2\tb\tD\t100\t1")
    assert(Market.computeOrder(operationTest1_to_Test2_cell))
    assert(Market.computeOrder(operationTest2_to_Test1_buy))
  }

  test("После 2ой сделки очереди покупки и продаж снова должны быть пустые") {
    assert(Market.dumpStackBuy().isEmpty)
    assert(Market.dumpStackCell().isEmpty)
  }

  test("После сделки у TEST1 должно быть по нулям, а у TEST2 - по одной штуке каждой бумаги") {
    val dump = Market.dumpClients()
    assert(dump.contains("TEST1\t1200\t0\t0\t0\t0"))
    assert(dump.contains("TEST2\t800\t1\t1\t1\t1"))
  }
}
