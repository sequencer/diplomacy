// See LICENSE.SiFive for license details.

package diplomacy

import chisel3.Data

/** Combinators for finding specific sets of [[LazyModule]]s/[[Node]]s.
  *
  * These can be used for e.g. finding specific TLBundles in a design and
  * placing monitors or annotating metadata.
  */
object aop {

  /** Contains information about an inward edge of a node */
  case class InwardEdge[Bundle <: Data, EdgeInParams](
    bundle: Bundle,
    edge:   EdgeInParams,
    node:   OutwardNode[_, _, Bundle])

  /** Contains information about an outward edge of a node */
  case class OutwardEdge[Bundle <: Data, EdgeOutParams](
    bundle: Bundle,
    edge:   EdgeOutParams,
    node:   InwardNode[_, _, Bundle])

  object Select {

    /** Collects the [[InwardEdge]]s of a node. Defined as a separate method so
      * that the bundle/edge types can be set properly
      */
    private def getInwardEdges[BI <: Data, EI](
      node: MixedNode[_, _, EI, BI, _, _, _, _ <: Data]
    ): Iterable[InwardEdge[BI, EI]] = {
      node.iPorts.zip(node.in).map {
        case ((_, node, _), (bundle, edge)) =>
          InwardEdge(bundle, edge, node)
      }
    }

    /** Applies the collect function to each [[InwardEdge]] of a node */
    def collectInwardEdges[T](node: BaseNode)(collect: PartialFunction[InwardEdge[_ <: Data, _], T]): Iterable[T] = {
      node match {
        case node: AnyMixedNode => getInwardEdges(node).collect(collect)
        case _ => Seq.empty
      }
    }

    /** Collects the [[OutwardEdge]]s of a node. Defined as a separate method so
      * that the bundle/edge types can be set properly
      */
    private def getOutwardEdges[BO <: Data, EO](
      node: MixedNode[_, _, _, _ <: Data, _, _, EO, BO]
    ): Iterable[OutwardEdge[BO, EO]] = {
      node.oPorts.zip(node.out).map {
        case ((_, node, _), (bundle, edge)) =>
          OutwardEdge(bundle, edge, node)
      }
    }

    /** Applies the collect function to each [[OutardEdge]] of a node */
    def collectOutwardEdges[T](node: BaseNode)(collect: PartialFunction[OutwardEdge[_ <: Data, _], T]): Iterable[T] = {
      node match {
        case node: AnyMixedNode => getOutwardEdges(node).collect(collect)
        case _ => Seq.empty
      }
    }

    /** Applies the collect function to a [[LazyModule]] and recursively to all
      * of its children.
      */
    def collectDeep[T](lmod: LazyModule)(collect: PartialFunction[LazyModule, T]): Iterable[T] = {
      collect.lift(lmod) ++
        lmod.getChildren.flatMap { child =>
          collectDeep(child)(collect)
        }
    }

    /** Applies the collect function to a [[LazyModule]] and its children if the
      * filter function returns true. Stops recursing when the filter function
      * returns false. e.g.
      * for this hierarchy
      *     A
      *    / \
      *   B   C
      *  / \   \
      * D   E   F
      *
      * the following select function
      * {{{
      * filterCollectDeep(A) {
      *   case B => false
      *   case _ => true
      * } { m =>
      *   printl(m)
      * }
      * }}}
      *
      * will only print modules A, C, and F
      */
    def filterCollectDeep[T](
      lmod:    LazyModule
    )(filter:  LazyModule => Boolean
    )(collect: PartialFunction[LazyModule, T]
    ): Iterable[T] = {
      if (filter(lmod)) {
        collect.lift(lmod) ++
          lmod.getChildren.flatMap { child =>
            filterCollectDeep(child)(filter)(collect)
          }
      } else {
        Iterable.empty
      }
    }
  }
}
