package support

import scala.concurrent.stm._

class MemoryImage[A, B](initial: A, update: (A, B) => A) {
  val current = Ref(initial)

  def apply(event: B)(implicit txn: InTxn): Unit = current.transform(update(_, event))
}
object MemoryImage {
  def apply[A, B](initial: A)(update: (A, B) => A) = new MemoryImage(initial, update)
}
