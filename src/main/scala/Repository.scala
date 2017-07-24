import scala.io.Source
import scala.language.dynamics

case class Client(name: String, usd: Long, a: Long, b: Long, c: Long, d: Long)
case class Request(clientname: String, operation: String, stock: String, price: Long, num: Long)
case class Transaction(buyer: String, seller: String, stock: String, price: Long, num: Long){
  def getStockQuantityByName(stockName: String) : Long = {
    if(stock==stockName) num else 0
  }
}




class Repository {

  val clientPattern = "([a-zA-Z0-9]+)\t([0-9]+)\t([0-9]+)\t([0-9]+)\t([0-9]+)\t([0-9]+)".r
  val orderPattern = "([a-zA-Z0-9]+)\t([b,s]+)\t([a-zA-Z]+)\t([0-9]+)\t([0-9]+)".r

  def getClients = {
    Source.fromInputStream(getClass.getResourceAsStream("clients.txt")).getLines.map(
      line=> {
        line match{
          case clientPattern(name,usd,a,b,c,d)=>{
            Client(name,usd.toLong,a.toLong,b.toLong,c.toLong,d.toLong)
          }
        }
      }).toList
  }

  def getOrders = {
    io.Source.fromInputStream(getClass.getResourceAsStream("orders.txt")).getLines.map(
        line=>{
          line match {
            case orderPattern(clientname, op, st, price, num) => {
              Request(clientname, op, st, price.toLong, num.toLong)
            }
          }
        }).toList
  }

}
