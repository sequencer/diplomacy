package diplomacy.nodes

import chisel3.Data
import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.SourceInfo

/** [[InwardNodeImp]] defines the types that describe the inward side of the [[BaseNode]].
  *
  * @tparam DI The type of the downward-flowing parameters received on the inner side of the node.
  * @tparam UI The type of the upward-flowing parameters generated by the inner side of the node.
  * @tparam EI The type of the diplomatically-resolved parameters for an Edge connected to the inner side of the node.
  * @tparam BI The type of the [[chisel3.Data]] (usually a [[chisel3.Bundle]]) used when connecting to the inner side of the node,
  *            corresponding to the real hardware interface that is emitted along the graph edge,
  *            generally parameterized by the [[EI]] type.
  */
trait InwardNodeImp[DI, UI, EI, BI <: Data] {

  /** Creates the inward edge parameters by combining the downward-flowing and upward-flowing parameters for edges
    * that connect to the inward side of this [[BaseNode]].
    *
    * It is left up to a user defining a particular protocol implementation to decide how the parameters flowing through
    * the graph in both directions on this Edge are combined into a single representation.
    *
    * @param pd         The downward-flowing parameters into the node along the edge.
    * @param pu         The upward-flowing parameters going out of the node along the edge.
    * @param p          A view of [[Parameters]] at the point at which the returned edge is being bound.
    * @param sourceInfo [[SourceInfo]] of this edge.
    * @return An inward edge of this node.
    */
  def edgeI(pd: DI, pu: UI, p: Parameters, sourceInfo: SourceInfo): EI

  /** Create an inward bundle parameterized by the inward edge.
    *
    * @param ei Inward edge of this node.
    * @return An outward Bundle of this node parameterized by the negotiated Edge parameters.
    */
  def bundleI(ei: EI): BI

  /** Defines how input parameters can be "mixed" or negotiated together.
    *
    * The default behavior is to just return `pu`.
    *
    * @param pu   The upward-flowing parameters going out of the node along the edge.
    * @param node An inward node to "mix" the upward-flowing parameters into.
    * @return Altered version of the upward-flowing parameters.
    */
  def mixI(pu: UI, node: InwardNode[DI, UI, BI]): UI = pu

  /** Function to generate and attach a monitor for this node input.
    *
    * @param bundle Inward bundle of this node to attach the monitor to.
    * @param edge   Edge of this node used to parameterize the bundle.
    */
  def monitor(bundle: BI, edge: EI): Unit = {}

  /** Define how the edge should be rendered (e.g. in GraphML).
    *
    * @param e Edge to render.
    * @return [[RenderedEdge]] description of how the edge should be generated.
    */
  def render(e: EI): RenderedEdge
}

/** [[OutwardNodeImp]] defines the types that describe the outwards side of the [[BaseNode]].
  *
  * @tparam DO The type of the downward-flowing parameters generated by the outer side of the node
  * @tparam UO Tye type of the upward-flowing parameters received by the outer side of the node
  * @tparam EO The type of the diplomatically-resolved parameters for an Edge connected to the outer side of the node.
  * @tparam BO The type of the [[chisel3.Data]] (usually a [[chisel3.Bundle]]) used when connecting to the outer side of the node,
  *            corresponding to the real hardware interface that is emitted along the graph edge,
  *            generally parameterized by the [[EO]] type.
  */
trait OutwardNodeImp[DO, UO, EO, BO <: Data] {

  /** Creates the outward edge parameters by combining the downward-flowing and upward-flowing parameters for edges
    * that connect to the outward side of this [[BaseNode]].
    *
    * It is left up to a user defining a particular protocol implementation to decide how the parameters flowing through
    * the graph in both directions on this Edge are combined into a single representation.
    *
    * @param pd         The downward-flowing parameters going out of the node along the edge.
    * @param pu         The upward-flowing parameters into the node along the edge.
    * @param p          A view of [[Parameters]] at the point at which the returned edge is being bound.
    * @param sourceInfo [[SourceInfo]] of this edge.
    * @return An outward edge of this node.
    */
  def edgeO(pd: DO, pu: UO, p: Parameters, sourceInfo: SourceInfo): EO

  /** Create an outward Bundle parameterized by the outward edge.
    *
    * @param eo Outward Edge of this node.
    * @return An outward Bundle of this node parameterized by the negotiated Edge parameters.
    */
  def bundleO(eo: EO): BO

  /** Defines how outward parameters can be "mixed" or negotiated together.
    *
    * The default behavior is to just return `pd`.
    *
    * @param pd   The downward-flowing parameters into the node along the edge.
    * @param node An outward node to "mix" the downward-flowing parameters into.
    * @return Altered version of the downward-flowing parameters.
    */
  def mixO(pd: DO, node: OutwardNode[DO, UO, BO]): DO = pd
}

/** The [[NodeImp]] combines an [[InwardNodeImp]] and an [[OutwardNodeImp]].
  *
  * This allows it to define whether it is a protocol-modifying (bridging) sort of node,
  * or whether it is an adapter type node that just modifies the parameters within a protocol.
  *
  * This class has no members and is solely used for holding type information.
  * Applications of diplomacy should extend [[NodeImp]] with a case object that sets concrete type arguments.
  *
  * @tparam D  Type of the downward-flowing parameters of the node.
  * @tparam U  Type of upward-flowing parameters of the node.
  * @tparam EO Type of the parameters describing an edge on the outer side of the node.
  * @tparam EI Type of the parameters describing an edge on the inner side of the node.
  * @tparam B  Bundle type generated on edges connecting to this node.
  */
abstract class NodeImp[D, U, EO, EI, B <: Data]
    extends Object
    with InwardNodeImp[D, U, EI, B]
    with OutwardNodeImp[D, U, EO, B]

/** A [[NodeImp]] where the inward and outward edge parameters are of the same type.
  *
  * If, in a given protocol implementation, the parameters visible to the node on the inward side of an edge are
  * the same as the parameters visible to the node on the outward side of an edge,
  * [[SimpleNodeImp]] can be used instead of [[NodeImp]].
  *
  * @tparam D Type of the downward-flowing parameters of the node.
  * @tparam U Type of the upward-flowing parameters of the node.
  * @tparam E Edge Parameters describing the connections on either side of the node.
  * @tparam B Bundle type generated on edges connecting to this node.
  */
abstract class SimpleNodeImp[D, U, E, B <: Data] extends NodeImp[D, U, E, E, B] {

  /** Creates the edge parameters by combining the downward-flowing and upward-flowing parameters for edges that connect to this node.
    *
    * It is left up to a user defining a particular protocol implementation to decide how the parameters flowing through the graph in
    * both directions are combined into a single representation on an Edge.
    *
    * @param pd         The downward-flowing parameters into the node along the edge.
    * @param pu         The upward-flowing parameters going out of the node along the edge.
    * @param p          [[Parameters]]s which can be used during negotiation.
    * @param sourceInfo [[SourceInfo]] of this edge.
    * @return Negotiated edge parameters.
    */
  def edge(pd: D, pu: U, p: Parameters, sourceInfo: SourceInfo): E

  def edgeO(pd: D, pu: U, p: Parameters, sourceInfo: SourceInfo): E = edge(pd, pu, p, sourceInfo)

  def edgeI(pd: D, pu: U, p: Parameters, sourceInfo: SourceInfo): E = edge(pd, pu, p, sourceInfo)

  /** Generate the Bundle from the negotiated Edge parameters.
    *
    * @param e the negotiated Edge parameters
    * @return the corresponding Bundle of this node
    */
  def bundle(e: E): B

  def bundleO(e: E): B = bundle(e)

  def bundleI(e: E): B = bundle(e)
}

/** [[RenderedEdge]] can set the color and label of the visualization of the DAG. */
case class RenderedEdge(
  colour:  String,
  label:   String = "",
  flipped: Boolean = false)