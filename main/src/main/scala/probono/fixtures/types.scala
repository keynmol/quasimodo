package quasimodo.fixtures

object types {
  type RecordedResults[A] = List[Either[Throwable, A]]
  type rec[F[_], A]       = F[RecordedResults[A]]
}
