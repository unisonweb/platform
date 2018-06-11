package org.unisonweb

import compilation._
import Term.Name

case class Environment(
  builtins: Map[Name, Computation],
  userDefined: Map[Hash, Computation],
  dataConstructors: Map[(Id,ConstructorId), Computation],
  effects: Map[(Id,ConstructorId), Computation]
)

object Environment {

  val standard = Environment(
    Builtins.builtins,
    userDefined = Map.empty,
    BuiltinTypes.dataConstructors,
    BuiltinTypes.effects)
}
