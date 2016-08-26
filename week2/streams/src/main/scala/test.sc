import streams.{GameDef, Solver, StringParserTerrain}

trait test extends GameDef with Solver with StringParserTerrain {

}

trait Level1 extends test {
  /* terrain for level 1*/

  val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

  val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
}

val level1 = new Level1 {
  pathsFromStart.toList
}