import market.{Client, Market, MarketOperation}
import org.scalatest._

class FailTest extends FunSuite {
  test("При попытке создать клиента по плохой строке - вернется false") {
    assert(!Market.createClient("TEST_FAIL\t1000\t130"))
  }

  test("При попытке напрямую создать клиента по плохой строке - вернется null") {
    assert(Client.newClientFromLine("TEST1\tTTT\t0\t1\tAS\tAS") == null)
  }

  test("При попытке создать заявку по плохой строке - вернется null") {
    assert(MarketOperation.newInstance("C1\ts\tD\tEEE\tEEE") == null)
  }


}
