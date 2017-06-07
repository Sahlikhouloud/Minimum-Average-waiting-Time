import scala.annotation.tailrec
import scala.collection.mutable

object MinimumAverageWaitingTime extends App {

  case class Customer(comingTime: Int, requiredTime: Int)

  private implicit val ordering = Ordering.fromLessThan[Customer](_.requiredTime > _.requiredTime)
  private val lines = io.Source.stdin.getLines()
  private val futureCustomerCount = lines.next.toInt
  private val futureCustomers: List[Customer] = (for (i <- 1 to futureCustomerCount) yield {
    val input = lines.next.split(" ")
    Customer(input.head.toInt, input.last.toInt)
  }).toList

  println(s"The minimum average waiting time is ${evaluateMinAverage(futureCustomers)}")

  @tailrec
  def evaluateMinAverage(futureCustomers: List[Customer],
                         waitingCustomers: mutable.PriorityQueue[Customer] = new mutable.PriorityQueue[Customer],
                         currentTime: Int = 0,
                         sumOfWaitingTime: Int = 0,
                         servedCustomerCount: Int = 0): Int = {
    val (futureComingCustomers, newComingCustomers) = futureCustomers.partition(_.comingTime > currentTime)
    if (futureComingCustomers.nonEmpty && newComingCustomers.isEmpty) {
      evaluateMinAverage(
        futureComingCustomers,
        waitingCustomers,
        currentTime + 1,
        sumOfWaitingTime,
        servedCustomerCount
      )
    } else {
      waitingCustomers.enqueue(newComingCustomers: _*)
      if (waitingCustomers.isEmpty) {
        if (servedCustomerCount == 0) 0
        else sumOfWaitingTime / servedCustomerCount
      } else {
        val nextCustomer = waitingCustomers.dequeue
        val servingTime = currentTime + nextCustomer.requiredTime

        evaluateMinAverage(
          futureComingCustomers,
          waitingCustomers,
          servingTime,
          sumOfWaitingTime + servingTime - nextCustomer.comingTime,
          servedCustomerCount + 1
        )
      }
    }
  }

}
