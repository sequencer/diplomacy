package diplomacyTest.config

import diplomacy.config._
import org.scalatest.flatspec.AnyFlatSpec

class ConfigSpec extends AnyFlatSpec {

  object K0 extends Field[String]("K0Default")

  object K1 extends Field[String]

  object K2 extends Field[String]

  object K3 extends Field[String]("K3Default")

  "if apply a not-existed key in parameter" should "throw" in {
    val pf0: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => "V0"
      }
    Parameters(pf0)(K1)
  }

  "if lift a empty key in parameter" should "throw" in {
    val pf0: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => "V0"
      }
    println(Parameters(pf0).lift(K1))
  }

  "by default, searching" should "start from end" in {
    val pf0: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => "V0"
      }
    val pf1: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => "V1"
      }
    val pf2: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => "V2"
      }
    val pf3: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => "V3"
      }
    println(Parameters(pf0).alter(pf1).alter(pf2).alter(pf3).apply(K0))
  }

  /** In example below `Parameters(pf0).alter(pf1)` is defined in:
    * {{{
    *           pf0         pf1         pf2         pf3
    * K0        V0          V1          V2          V3
    * K1                                site(K0)
    * K2                                here(K1)
    * K3                                  up(K2)
    * }}}
    */
  "site, here, up" should "be found" in {
    val pf0: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => "V0"
      }
    val pf1: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => "V1"
      }
    val pf2: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => "V2"
        case K1 => site(K0)
        case K2 => here(K0)
        case K3 => up(K0)
      }
    val pf3: (View, View, View) => PartialFunction[Any, Any] = { (site: View, here: View, up: View) =>
      {
        case K0 => "V3"
      }
    }
    val parameters = Parameters(pf0).alter(pf1).alter(pf2).alter(pf3)

    println(parameters(K1))
    println(parameters(K2))
    println(parameters(K3))
  }

  /** {{{
    *           pf0
    * K0        here(K0)
    * }}}
    */
  "recursive find a key" should "Stack Overflow" in {
    val pf0: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => here(K0)
      }
    Parameters(pf0).lift(K0)
  }

  /** {{{
    *           pf0         pf1
    * K0        V0          up(K0)
    * K1                  here(K0)
    * }}}
    */
  "V0" should "be found" in {
    val pf0: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        case K0 => "V0"
      }
    val pf1: (View, View, View) => PartialFunction[Any, Any] =
      (site: View, here: View, up: View) => {
        // search in previous [[PartialFunction]]
        case K0 => up(K0)
        // search in this [[PartialFunction]]
        case K1 => here(K0)
      }

    // construct a Parameter from [[pf0]]
    println(
      Parameters(pf0)
        // append [[pf1]] to Parameter chain
        .alter(Parameters(pf1))
        .alter(Parameters(pf1))
        .alter(Parameters(pf1))
        .alter(Parameters(pf1))
        // search key [[K1]]
        .lift(K1)
    )
  }
}
