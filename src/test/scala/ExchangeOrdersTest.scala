import org.scalatest.FunSuite

class ExchangeOrdersTestSuite extends FunSuite{

  implicit val transactionOrdering = new Ordering[Transaction]{
    override def compare(x: Transaction, y: Transaction): Int = {
      x.toString.compareTo(y.toString)
    }
  }

  implicit val clientOrdering = new Ordering[Client]{
    override def compare(x: Client, y: Client): Int = {
      x.toString.compareTo(y.toString)
    }
  }

  test("Test get quantity of stock in transaction by stock name"){
    val transaction = Transaction("C1","C2","C",10,5)
    assert(transaction.getStockQuantityByName("A")==0)
    assert(transaction.getStockQuantityByName("C")==5)
    assert(transaction.getStockQuantityByName("D")==0)
    assert(transaction.getStockQuantityByName("B")==0)
  }

  test("Test creating transaction list from requests"){
    val clients = List(Client("C1",1000,130,240,760,320),Client("C2",1000,130,240,760,320),Client("C3",1000,130,240,760,320))
    val requests = List(Request("C1","b","A",1500,1),
                      Request("C1","s","A",1500,1),
                      Request("C1","b","C",15,4),
                      Request("C8","s","D",20,5),
                      Request("C1","s","C",15,4),
                      Request("C3","s","A",1500,1),
                      Request("C2","s","C",15,4),
                      Request("C5","b","D",20,5),
                      )
    val result = OrderRequest.createTransactions(requests)
    assert(result.sorted.sameElements(List(Transaction("C5","C8","D",20,5), Transaction("C1","C2","C",15,4), Transaction("C1","C3","A",1500,1)).sorted))
  }

  test("Test execute transactions"){
    val clients = List(Client("C1",1000,130,240,760,320),Client("C4",123,5,5,5,5),Client("C2",1000,130,240,760,320),Client("C3",1000,130,240,760,320))
    val requests = List(
      Request("C1","b","A",1500,1),
      Request("C1","s","A",1500,1),
      Request("C1","b","C",15,4),
      Request("C2","s","D",20,5),
      Request("C1","s","C",15,4),
      Request("C3","s","A",1500,1),
      Request("C3","s","C",15,4),
      Request("C1","b","D",20,5))
    val result = OrderRequest.executeOrders(clients, OrderRequest.createTransactions(requests))._1.sorted
    assert(result.sameElements(List(Client("C4",123,5,5,5,5),Client("C1",-660,131,240,764,325), Client("C2",1100,130,240,760,315), Client("C3",2560,129,240,756,320)).sorted))
  }

}
