package support

trait EventDescriptor[-Event] {
  def streamId(event: Event): String
}
object EventDescriptor {
  def apply[Event](id: Event => Identifier): EventDescriptor[Event] = new EventDescriptor[Event] {
    override def streamId(event: Event) = id(event).toString
  }
}
