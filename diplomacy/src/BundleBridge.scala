// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.experimental.{DataMirror,IO}
import freechips.rocketchip.config.{Parameters,Field}

case class BundleBridgeParams[T <: Data](genOpt: Option[() => T])

case object BundleBridgeParams {
  def apply[T <: Data](gen: () => T): BundleBridgeParams[T] = BundleBridgeParams(Some(gen))
}

case class BundleBridgeEdgeParams[T <: Data](source: BundleBridgeParams[T], sink: BundleBridgeParams[T])

class BundleBridgeImp[T <: Data]() extends SimpleNodeImp[BundleBridgeParams[T], BundleBridgeParams[T], BundleBridgeEdgeParams[T], T]
{
  def edge(pd: BundleBridgeParams[T], pu: BundleBridgeParams[T], p: Parameters, sourceInfo: SourceInfo) = BundleBridgeEdgeParams(pd, pu)
  def bundle(e: BundleBridgeEdgeParams[T]): T = {
    val sourceOpt = e.source.genOpt.map(_())
    val sinkOpt = e.sink.genOpt.map(_())
    (sourceOpt, sinkOpt) match {
      case (None,    None)    =>
        throw new Exception("BundleBridge needs source or sink to provide bundle generator function")
      case (Some(a), None)    => a
      case (None,    Some(b)) => b
      case (Some(a), Some(b)) => {
        require(DataMirror.checkTypeEquivalence(a, b),
          s"BundleBridge requires doubly-specified source and sink generators to have equivalent Chisel Data types, but got \n$a\n vs\n$b")
        a
      }
    }
  }
  def render(e: BundleBridgeEdgeParams[T]) = RenderedEdge(colour = "#cccc00" /* yellow */)
}

case class BundleBridgeSink[T <: Data](genOpt: Option[() => T] = None)
                                      (implicit valName: ValName)
  extends SinkNode(new BundleBridgeImp[T])(Seq(BundleBridgeParams(genOpt)))
{
  def bundle: T = in(0)._1

  def makeIO()(implicit valName: ValName): T = makeIOs()(valName).head
  def makeIO(name: String): T = makeIOs()(ValName(name)).head
}

case class BundleBridgeSource[T <: Data](genOpt: Option[() => T] = None)(implicit valName: ValName) extends SourceNode(new BundleBridgeImp[T])(Seq(BundleBridgeParams(genOpt)))
{
  def bundle: T = out(0)._1

  def makeIO()(implicit valName: ValName): T = makeIOs()(valName).head
  def makeIO(name: String): T = makeIOs()(ValName(name)).head

  private var doneSink = false
  def makeSink()(implicit p: Parameters) = {
    require (!doneSink, "Can only call makeSink() once")
    doneSink = true
    val sink = BundleBridgeSink[T]()
    sink := this
    sink
  }
}

case object BundleBridgeSource {
  def apply[T <: Data](gen: () => T)(implicit valName: ValName): BundleBridgeSource[T] = {
    BundleBridgeSource(Some(gen))
  }
}

case class BundleBridgeIdentityNode[T <: Data]()(implicit valName: ValName) extends IdentityNode(new BundleBridgeImp[T])()
case class BundleBridgeEphemeralNode[T <: Data]()(implicit valName: ValName) extends EphemeralNode(new BundleBridgeImp[T])()

case class BundleBridgeNexusNode[T <: Data](default: Option[() => T] = None)
                                           (implicit valName: ValName)
  extends NexusNode(new BundleBridgeImp[T])(
    dFn = seq => seq.headOption.getOrElse(BundleBridgeParams(default)),
    uFn = seq => seq.headOption.getOrElse(BundleBridgeParams(default)),
    inputRequiresOutput = false, // enables publishers with no subscribers
    outputRequiresInput = !default.isDefined)

class BundleBridgeNexus[T <: Data](
  inputFn: Seq[T] => T,
  outputFn: (T, Int) => Seq[T],
  default: Option[() => T] = None
) (implicit p: Parameters) extends LazyModule
{
  val node = BundleBridgeNexusNode[T](default)

  lazy val module = new LazyModuleImp(this) {
    val defaultWireOpt = default.map(_())
    val inputs: Seq[T] = defaultWireOpt.toList ++ node.in.map(_._1)
    require(inputs.size >= 1, "BundleBridgeNexus requires at least one input or default.")
    inputs.foreach { i => require(DataMirror.checkTypeEquivalence(i, inputs.head),
      s"BundleBridgeNexus requires all inputs have equivalent Chisel Data types, but got\n$i\nvs\n${inputs.head}")
    }
    def getElements(x: Data): Seq[Element] = x match {
      case e: Element => Seq(e)
      case a: Aggregate => a.getElements.flatMap(getElements)
    }
    inputs.flatMap(getElements).foreach { elt => DataMirror.directionOf(elt) match {
      case ActualDirection.Output => ()
      case ActualDirection.Unspecified => ()
      case _ => require(false, "BundleBridgeNexus can only be used with Output-directed Bundles")
    } }

    val broadcast: T = inputFn(inputs)
    val outputs: Seq[T] = outputFn(broadcast, node.out.size)
    node.out.map(_._1).foreach { o => require(DataMirror.checkTypeEquivalence(o, outputs.head),
      s"BundleBridgeNexus requires all outputs have equivalent Chisel Data types, but got\n$o\nvs\n${outputs.head}")
    }
    require(outputs.size == node.out.size,
      s"BundleBridgeNexus outputFn must generate one output wire per edgeOut, but got ${outputs.size} vs ${node.out.size}")

    node.out.zip(outputs).foreach { case ((out, _), bcast) => out := bcast }
  }
}

object BundleBridgeNexus {
  def requireOne[T <: Data](registered: Boolean)(seq: Seq[T]): T = {
    require(seq.size == 1, "BundleBroadcast default requires one input")
    if (registered) RegNext(seq.head) else seq.head
  }

  def orReduction[T <: Data](registered: Boolean)(seq: Seq[T]): T = {
    val x = seq.reduce((a,b) => (a.asUInt | b.asUInt).asTypeOf(seq.head))
    if (registered) RegNext(x) else x
  }

  def fillN[T <: Data](registered: Boolean)(x: T, n: Int): Seq[T] = Seq.fill(n) {
    if (registered) RegNext(x) else x
  }

  def apply[T <: Data](
    inputFn: Seq[T] => T = orReduction[T](false) _,
    outputFn: (T, Int) => Seq[T] = fillN[T](false) _,
    default: Option[() => T] = None
  )(implicit p: Parameters, valName: ValName): BundleBridgeNexusNode[T] = {
    val broadcast = LazyModule(new BundleBridgeNexus[T](inputFn, outputFn, default))
    broadcast.node
  }
}

object BundleBroadcast {
  def apply[T <: Data](
    name: Option[String] = None,
    registered: Boolean = false
  )(implicit p: Parameters, valName: ValName): BundleBridgeNexusNode[T] = {
    val finalName = name.map(ValName(_)).getOrElse(valName)
    BundleBridgeNexus.apply[T](
      inputFn = BundleBridgeNexus.requireOne[T](registered) _,
      outputFn = BundleBridgeNexus.fillN[T](registered) _)(
      p, finalName)
  }
}
