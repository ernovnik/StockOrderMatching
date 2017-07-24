import scala.annotation.tailrec

object OrderRequest {

  def createTransactions(orders: List[Request]) : List[Transaction] = {
    val buySellPairs = findMatchingOrders(orders.filter(o=>o.operation=="b"), orders.filter(o=>o.operation=="s"), Nil)
    createTransactionsFromPairs(buySellPairs._1)
  }

  def createTransactionsFromPairs(pairs: List[(Request,Request)]) = {
    pairs.map(pair=>Transaction(pair._1.clientname, pair._2.clientname, pair._1.stock, pair._1.price, pair._1.num))
  }

  @tailrec
  def findMatchingOrders(buys: List[Request], sells: List[Request], pairs: List[(Request,Request)]) : (List[(Request,Request)], List[Request], List[Request]) = {
    buys match{
      case Nil=>(pairs, buys, sells)
      case head :: tail=> {
        val sellOption = sells.find(sell=>sell.stock==head.stock&&sell.price==head.price&&sell.num==head.num&&sell.clientname!=head.clientname)
        sellOption match{
          case Some(sell)=> findMatchingOrders(buys.tail,sells.filter(_!=sell),(head,sell)::pairs)
          case None => findMatchingOrders(buys.tail,sells, pairs)
        }
      }
    }
  }

  @tailrec
  def executeOrders(co: (List[Client],List[Transaction])) : (List[Client],List[Transaction]) = {
        co match {
          case (Nil, orders) => co
          case (_, Nil) => co
          case (clients, orders) => {
          val transaction = orders.head
            val buyerOpt = clients.find(client=>client.name==transaction.buyer)
            val sellerOpt = clients.find(client=>client.name==transaction.seller)
            (buyerOpt,sellerOpt) match{
              case (Some(buyer),Some(seller))=>{
                val buyer_index = clients.indexOf(buyer)
                val seller_index = clients.indexOf(seller)
                val amountusd = transaction.price * transaction.num
                val Client(name,usd,a,b,c,d) = buyer
                val newBuyer = Client(name, usd - amountusd, a + transaction.getStockQuantityByName("A"), b + transaction.getStockQuantityByName("B"), c + transaction.getStockQuantityByName("C"), d + transaction.getStockQuantityByName("D"))
                val Client(name1,usd1,a1,b1,c1,d1) = seller
                val newSeller = Client(name1, usd1 + amountusd, a1 - transaction.getStockQuantityByName("A"), b1- transaction.getStockQuantityByName("B"), c1 - transaction.getStockQuantityByName("C"), d1 - transaction.getStockQuantityByName("D"))
                executeOrders((co._1.updated(buyer_index, newBuyer).updated(seller_index,newSeller),co._2.tail))
              }
              case _=> executeOrders(co._1,co._2.tail)
            }
          }
        }
  }
}
