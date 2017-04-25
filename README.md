### Iota
[![Build Status](https://api.travis-ci.org/47deg/iota.png?branch=master)](https://travis-ci.org/47deg/iota)

### Introduction

Iota is a tiny framework for fast product and coproduct types.

Unlike many coproduct implementations that use a linked list at the
value level, Iota stores indexes for coproduct values to allow for
quick access.

At the type level, Iota uses a linked list type with a binary type
operator for a simple syntax.

```scala
import iota._
import TList.::
import KList.:::

// a coproduct of types
type Foo = Cop[Int :: String :: Double :: TNil]

// a coproduct of type constructors
type Bar[A] = CopK[Option ::: List ::: Seq ::: KNil, A]
```

An `Free` example is available [in the tests][free example].

### Documentation

Documetation coming soon.

### License
The license can be found in [COPYING].

[COPYING]: COPYING
[free example]: modules/core/src/test/scala/iotatests/FreeCopKTests.scala
