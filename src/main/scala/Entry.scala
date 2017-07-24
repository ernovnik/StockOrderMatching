import java.io._

object Entry extends App {



  val repository = new Repository
  val clients = repository.getClients
  val orders = repository.getOrders
  val pairs = OrderRequest.createTransactions(orders)
  val orderRequest = OrderRequest.executeOrders((clients, pairs))
  val file = new File("result.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  orderRequest._1.foreach(client => {
    bw.write(s"${client.name} ${client.usd} ${client.a} ${client.b} ${client.c} ${client.d}")
    bw.newLine()
  }
  )
  bw.close()
}
