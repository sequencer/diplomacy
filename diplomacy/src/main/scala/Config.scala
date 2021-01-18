// Copyright 2016-2019 SiFive, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package diplomacy


/** CDE is a key-value based data structure.
  * It use [[PartialFunction]] to store parameters.
  *
  * It use [[config.Field]] to define a key,
  *
  * @example
  * {{{
  *   case object SomeKey extends Field[String]("ValueNameOfThisKey")
  * }}}
  * In the internal data structure, it `SomeKey` is defined as an optional String.
  *
  *
  * [[config.Parameters]] is a linked list, a element is a function generating a [[PartialFunction]].
  * This function is defined as
  * {{{
  *   (site: Parameters, here: Parameters, up: Parameters) => PartialFunction[Any, Any])
  * }}}
  * for a "case" in the PartialFunction, Field[T] is the key, and T is the type of a specific value.
  *
  * site : from end to beginning, search a key in all elements of config list.
  * here : search a key in the current element.
  * up : search a key from previous element of this element.
  *
  *
  *
  * @note
  * Implementation details
  * for a Parameter in depth 3:
  * {{{
  *           pf0         pf1         pf2
  * K0        V0          up(K0)      V1
  * K1                  site(K2)      V2
  * K2                  here(K0)      V3
  * }}}
  *
  *
  *
  */
object config {
  /** User defined key. */
  abstract class Field[T](private[config] val default: Option[T]){
    def this() = this(None)
    def this(default: T) = this(Some(default))
  }

  /** Data structure of a parameter to query a [[Field]]. */
  abstract class View {
    /** query key `pname` from `this` View. */
    final def apply[T](pname: Field[T]): T = apply(pname, this)
    /** query key `pname` from `site`.
      *
      * like [[PartialFunction.apply]], it return the value in type `T` directly.
      * @throws IllegalArgumentException if value is not found.
      * @return value corresponds to `pname`.
      */
    final def apply[T](pname: Field[T], site: View): T = {
      val out = find(pname, site)
      require(out.isDefined, s"Key ${pname} is not defined in Parameters")
      out.get
    }

    /** lift from `this`. */
    final def lift[T](pname: Field[T]): Option[T] = lift(pname, this)
    /** lift from `site`.
      *
      * like [[PartialFunction.lift]], it return a [[Option]] type of `T`.
      * @return T queried value.
      */
    final def lift[T](pname: Field[T], site: View): Option[T] = find(pname, site).map(_.asInstanceOf[T])

    /** internal API to define the behavior to query a key.
      * In [[TerminalView]], return `pname.default`
      * In [[ChainView]], use [[Parameters.chain]] to query.
      */
    protected[config] def find[T](pname: Field[T], site: View): Option[T]
  }

  /** User API to construct a key-value data structure. */
  abstract class Parameters extends View {
    /** settings in 'rhs' overrule settings in 'this'.
      * @example todo
      */
    final def alter(rhs: Parameters): Parameters =
      new ChainParameters(rhs, this)

    /** construct a [[Parameters]] based on function to generate the [[PartialFunction]] defined in 'f',
      * overrule settings in 'this'.
      * @example todo
      */
    final def alter(f: (View, View, View) => PartialFunction[Any, Any]): Parameters =
      alter(Parameters(f))

    /** construct a [[Parameters]] based on [[PartialFunction]] defined in 'f',
      * overrule settings in 'this'.
      */
    final def alterPartial(f: PartialFunction[Any, Any]): Parameters =
      alter(Parameters((_, _, _) => f))

    /** construct a [[Parameters]] based on [[Map]] defined in 'm',
      * overrule settings in 'this'.
      */
    final def alterMap(m: Map[Any, Any]): Parameters =
      alter(new MapParameters(m))

    /** settings in 'this' overrule settings in 'x'.
      * @example todo
      */
    final def orElse(x: Parameters): Parameters = x.alter(this)

    @deprecated("Please use 'alter' or 'orElse' instead of '++'.")
    final def ++(x: Parameters): Parameters = orElse(x)

    protected[config] def chain[T](site: View, tail:     View, pname: Field[T]): Option[T]
    protected[config] def find[T](pname: Field[T], site: View): Option[T] = chain(site, new TerminalView, pname)
  }

  object Parameters {
    /** construct an empty parameter. */
    def empty: Parameters = new EmptyParameters
    /** construct a [[Parameters]] based on function to generate the [[PartialFunction]] defined in 'f'. */
    def apply(f: (View, View, View) => PartialFunction[Any, Any]): Parameters = new PartialParameters(f)
  }

  class Config(p: Parameters) extends Parameters {
    /** construct a [[Config]] based on function to generate the [[PartialFunction]] defined in 'f'. */
    def this(f: (View, View, View) => PartialFunction[Any, Any]) = this(Parameters(f))

    protected[config] def chain[T](site: View, tail: View, pname: Field[T]): Option[T] = p.chain(site, tail, pname)
    override def toString: String = this.getClass.getSimpleName
    def toInstance: Config = this
  }

  // Internal implementation:
  /** View to return a default value. */
  private class TerminalView extends View {
    def find[T](pname: Field[T], site: View): Option[T] = pname.default
  }

  /** View to chain `tail` after `head`. */
  private class ChainView(head: Parameters, tail: View) extends View {
    def find[T](pname: Field[T], site: View) = head.chain(site, tail, pname)
  }

  /** Parameters to chain 'y' after 'x'. */
  private class ChainParameters(x: Parameters, y: Parameters) extends Parameters {
    def chain[T](site: View, tail: View, pname: Field[T]) = x.chain(site, new ChainView(y, tail), pname)
  }

  /** Parameters to chain `tail` after `head`. */
  private class EmptyParameters extends Parameters {
    def chain[T](site: View, tail: View, pname: Field[T]) = tail.find(pname, site)
  }

  private class PartialParameters(f: (View, View, View) => PartialFunction[Any, Any]) extends Parameters {
    protected[config] def chain[T](site: View, tail: View, pname: Field[T]): Option[T] = {
      val g = f(site, new ChainView(this, tail), tail)
      if (g.isDefinedAt(pname))
        // search here
        Some(g.apply(pname).asInstanceOf[T])
      else
        // search tail
        tail.find(pname, site)
    }
  }

  private class MapParameters(map: Map[Any, Any]) extends Parameters {
    protected[config] def chain[T](site: View, tail: View, pname: Field[T]) = {
      val g = map.get(pname)
      if (g.isDefined)
        Some(g.get.asInstanceOf[T])
      else
        tail.find(pname, site)
    }
  }
}
