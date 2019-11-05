import sbt._

object Dependencies {
  val catsEffectV = "2.0.0"
  val catsEffect  = "org.typelevel" %% "cats-effect" % catsEffectV

  val kindProjectorV = "0.9.8"
  val kindProjector  = "org.spire-math" %% "kind-projector" % kindProjectorV

  val expectyV = "0.13.0"
  val expecty  = "com.eed3si9n.expecty" %% "expecty" % expectyV

  val scalacheckV = "1.14.1"
  val scalacheck  = "org.scalacheck" %% "scalacheck" % scalacheckV
}
