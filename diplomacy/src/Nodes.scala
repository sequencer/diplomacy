// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import chisel3.internal.sourceinfo.SourceInfo

// DI = Downwards flowing Parameters received on the inner side of the node
// UI = Upwards   flowing Parameters generated by the inner side of the node
// EI = Edge Parameters describing a connection on the inner side of the node
// BI = Bundle type used when connecting to the inner side of the node
trait InwardNodeImp[DI, UI, EI, BI <: Data]
{
  def edgeI(pd: DI, pu: UI): EI
  def bundleI(ei: Seq[EI]): Vec[BI]
  def mixI(pu: UI, node: InwardNode[DI, UI, BI]): UI = pu
  def connect(bo: => BI, bi: => BI, e: => EI)(implicit sourceInfo: SourceInfo): (Option[LazyModule], () => Unit)
}

// DO = Downwards flowing Parameters generated by the outner side of the node
// UO = Upwards   flowing Parameters received on the outner side of the node
// EO = Edge Parameters describing a connection on the outer side of the node
// BO = Bundle type used when connecting to the outer side of the node
trait OutwardNodeImp[DO, UO, EO, BO <: Data]
{
  def edgeO(pd: DO, pu: UO): EO
  def bundleO(eo: Seq[EO]): Vec[BO]
  def mixO(pd: DO, node: OutwardNode[DO, UO, BO]): DO = pd
}

abstract class NodeImp[D, U, EO, EI, B <: Data]
  extends Object with InwardNodeImp[D, U, EI, B] with OutwardNodeImp[D, U, EO, B]

abstract class BaseNode
{
  // You cannot create a Node outside a LazyModule!
  require (!LazyModule.stack.isEmpty)

  val lazyModule = LazyModule.stack.head
  val index = lazyModule.nodes.size
  lazyModule.nodes = this :: lazyModule.nodes

  def name = lazyModule.name + "." + getClass.getName.split('.').last
  def colour = "blue"
  def omitGraphML = outputs.isEmpty && inputs.isEmpty

  protected[tilelink2] def outputs: Seq[BaseNode]
  protected[tilelink2] def inputs:  Seq[BaseNode]
}

trait InwardNode[DI, UI, BI <: Data] extends BaseNode
{
  protected[tilelink2] val numPI: Range.Inclusive
  require (!numPI.isEmpty, s"No number of inputs would be acceptable to ${name}${lazyModule.line}")
  require (numPI.start >= 0, s"${name} accepts a negative number of inputs${lazyModule.line}")

  private val accPI = ListBuffer[(Int, OutwardNode[DI, UI, BI])]()
  private var iRealized = false

  protected[tilelink2] def iPushed = accPI.size
  protected[tilelink2] def iPush(index: Int, node: OutwardNode[DI, UI, BI])(implicit sourceInfo: SourceInfo) {
    val info = sourceLine(sourceInfo, " at ", "")
    val noIs = numPI.size == 1 && numPI.contains(0)
    require (!noIs, s"${name}${lazyModule.line} was incorrectly connected as a sink" + info)
    require (!iRealized, s"${name}${lazyModule.line} was incorrectly connected as a sink after it's .module was used" + info)
    accPI += ((index, node))
  }

  private def reqI() = require(numPI.contains(accPI.size), s"${name} has ${accPI.size} inputs, expected ${numPI}${lazyModule.line}")
  protected[tilelink2] lazy val iPorts = { iRealized = true; reqI(); accPI.result() }

  protected[tilelink2] val iParams: Seq[UI]
  protected[tilelink2] def iConnect: Vec[BI]
}

trait OutwardNode[DO, UO, BO <: Data] extends BaseNode
{
  protected[tilelink2] val numPO: Range.Inclusive
  require (!numPO.isEmpty, s"No number of outputs would be acceptable to ${name}${lazyModule.line}")
  require (numPO.start >= 0, s"${name} accepts a negative number of outputs${lazyModule.line}")

  private val accPO = ListBuffer[(Int, InwardNode [DO, UO, BO])]()
  private var oRealized = false

  protected[tilelink2] def oPushed = accPO.size
  protected[tilelink2] def oPush(index: Int, node: InwardNode [DO, UO, BO])(implicit sourceInfo: SourceInfo) {
    val info = sourceLine(sourceInfo, " at ", "")
    val noOs = numPO.size == 1 && numPO.contains(0)
    require (!noOs, s"${name}${lazyModule.line} was incorrectly connected as a source" + info)
    require (!oRealized, s"${name}${lazyModule.line} was incorrectly connected as a source after it's .module was used" + info)
    accPO += ((index, node))
  }

  private def reqO() = require(numPO.contains(accPO.size), s"${name} has ${accPO.size} outputs, expected ${numPO}${lazyModule.line}")
  protected[tilelink2] lazy val oPorts = { oRealized = true; reqO(); accPO.result() }

  protected[tilelink2] val oParams: Seq[DO]
  protected[tilelink2] def oConnect: Vec[BO]
}

class MixedNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data](
  inner: InwardNodeImp [DI, UI, EI, BI],
  outer: OutwardNodeImp[DO, UO, EO, BO])(
  private val dFn: (Int, Seq[DI]) => Seq[DO],
  private val uFn: (Int, Seq[UO]) => Seq[UI],
  protected[tilelink2] val numPO: Range.Inclusive,
  protected[tilelink2] val numPI: Range.Inclusive)
  extends BaseNode with InwardNode[DI, UI, BI] with OutwardNode[DO, UO, BO]
{
  // meta-data for printing the node graph
  protected[tilelink2] def outputs = oPorts.map(_._2)
  protected[tilelink2] def inputs  = iPorts.map(_._2)

  private def reqE(o: Int, i: Int) = require(i == o, s"${name} has ${i} inputs and ${o} outputs; they must match${lazyModule.line}")
  protected[tilelink2] lazy val oParams: Seq[DO] = {
    val o = dFn(oPorts.size, iPorts.map { case (i, n) => n.oParams(i) })
    reqE(oPorts.size, o.size)
    o.map(outer.mixO(_, this))
  }
  protected[tilelink2] lazy val iParams: Seq[UI] = {
    val i = uFn(iPorts.size, oPorts.map { case (o, n) => n.iParams(o) })
    reqE(i.size, iPorts.size)
    i.map(inner.mixI(_, this))
  }

  lazy val edgesOut = (oPorts zip oParams).map { case ((i, n), o) => outer.edgeO(o, n.iParams(i)) }
  lazy val edgesIn  = (iPorts zip iParams).map { case ((o, n), i) => inner.edgeI(n.oParams(o), i) }

  lazy val bundleOut = outer.bundleO(edgesOut)
  lazy val bundleIn  = inner.bundleI(edgesIn)

  def oConnect = bundleOut
  def iConnect = bundleIn

  // connects the outward part of a node with the inward part of this node
  def := (y: OutwardNode[DI, UI, BI])(implicit sourceInfo: SourceInfo): Option[LazyModule] = {
    val x = this // x := y
    val info = sourceLine(sourceInfo, " at ", "")
    require (!LazyModule.stack.isEmpty, s"${y.name} cannot be connected to ${x.name} outside of LazyModule scope" + info)
    val i = x.iPushed
    val o = y.oPushed
    y.oPush(i, x)
    x.iPush(o, y)
    val (out, binding) = inner.connect(y.oConnect(o), x.iConnect(i), x.edgesIn(i))
    LazyModule.stack.head.bindings = binding :: LazyModule.stack.head.bindings
    out
  }
}

class SimpleNode[D, U, EO, EI, B <: Data](imp: NodeImp[D, U, EO, EI, B])(
  oFn: (Int, Seq[D]) => Seq[D],
  iFn: (Int, Seq[U]) => Seq[U],
  numPO: Range.Inclusive,
  numPI: Range.Inclusive)
    extends MixedNode[D, U, EI, B, D, U, EO, B](imp, imp)(oFn, iFn, numPO, numPI)

class IdentityNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])
  extends SimpleNode(imp)({case (_, s) => s}, {case (_, s) => s}, 0 to 999, 0 to 999)

class OutputNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B]) extends IdentityNode(imp)
{
  override def oConnect = bundleOut
  override def iConnect = bundleOut
}

class InputNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B]) extends IdentityNode(imp)
{
  override def oConnect = bundleIn
  override def iConnect = bundleIn
}

class SourceNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])(po: PO, num: Range.Inclusive = 1 to 1)
  extends SimpleNode(imp)({case (n, Seq()) => Seq.fill(n)(po)}, {case (0, _) => Seq()}, num, 0 to 0)
{
  require (num.end >= 1, s"${name} is a source which does not accept outputs${lazyModule.line}")
}

class SinkNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])(pi: PI, num: Range.Inclusive = 1 to 1)
  extends SimpleNode(imp)({case (0, _) => Seq()}, {case (n, Seq()) => Seq.fill(n)(pi)}, 0 to 0, num)
{
  require (num.end >= 1, s"${name} is a sink which does not accept inputs${lazyModule.line}")
}

class InteriorNode[PO, PI, EO, EI, B <: Data](imp: NodeImp[PO, PI, EO, EI, B])
  (oFn: Seq[PO] => PO, iFn: Seq[PI] => PI, numPO: Range.Inclusive, numPI: Range.Inclusive)
  extends SimpleNode(imp)({case (n,s) => Seq.fill(n)(oFn(s))}, {case (n,s) => Seq.fill(n)(iFn(s))}, numPO, numPI)
{
  require (numPO.end >= 1, s"${name} is an adapter which does not accept outputs${lazyModule.line}")
  require (numPI.end >= 1, s"${name} is an adapter which does not accept inputs${lazyModule.line}")
}
