package y2021.day12

import y2021.day08.Day08a.readInput

case class Cave(name: String) {

  def isSmall: Boolean = {
    !name.exists(c => c.isUpper)
  }

}

/**
 * A path from one cave to another.
 *
 * Note that a path can be traversed in both directions, so it counts as two paths.
 *
 * @param from the start cave
 * @param to the end cave
 */
case class Path(from: Cave, to: Cave)

object Path {

  def parseLine(line: String): Path = {
    val parts = line.split("-")
    if (parts.length != 2) throw new IllegalArgumentException
    Path(Cave(parts(0)), Cave(parts(1)))
  }
}

object Day12a extends App {

  /**
   * Create a map of a path.
   *
   * The map will have to entries, because the path is traversable in two directions.
   *
   * @param path the path
   * @return the map
   */
  def pathMap(path: Path): Map[Cave, Cave] = {
    Map(path.from -> path.to, path.to -> path.from)
  }

  /**
   * Create a combined path map from the file input.
   *
   * @param lines the input, each line representing a single [[Path]].
   * @return the map modeling the entire cave system.
   */
  def pathMap(lines: List[String]): Map[Cave, List[Cave]] = {

    lines
      .map(line => Path.parseLine(line))
      .map(p => pathMap(p))
      .foldLeft(Map[Cave, List[Cave]]())((acc, map) => {
        map.foldLeft(acc)((acc, p) => p match {
          case (from, to) =>
            acc + (from -> acc.getOrElse(from, List[Cave]()).appended(to))
        })
      })
  }

  /**
   * Remove a cave from the route map.
   *
   * The cave is removed both as a starting poind and as a destination.
   *
   * If that leaves a starting cave without any destinations, it is completely removed.
   *
   * @param routes the route map
   * @param toRemove the cave to remove
   * @return an adjusted route map
   */
  def removeCave(routes: Map[Cave, List[Cave]], toRemove: Cave): Map[Cave, List[Cave]] = {

    def removeCaveFromDestinations(entry: (Cave, List[Cave])): (Cave, List[Cave]) = {
      entry match {
        case (key, value) => key -> value.filterNot(c => c == toRemove)
      }
    }
    val withoutKey = routes - toRemove
    (withoutKey map removeCaveFromDestinations).filter(entry => entry match {
      case (_, Nil) => false
      case (_, _) => true
    })
  }

  /**
   * Determine all the paths through the cave system that begin in the 'start' cave and lead to the 'end' cave.
   * @param routes the route map
   * @return the paths, represented as a list of caves.
   */
  def getAllPaths(routes: Map[Cave, List[Cave]]): List[List[Cave]] = {
    val startCave = Cave("start")
    val endCave = Cave("end")

    // Add a cave to all paths found so far
    def addCaveToPath(paths: List[List[Cave]], cave: Cave, routesLeft: Map[Cave, List[Cave]]): List[List[Cave]] = {
      // Updated paths, with the cave added
      val newPaths = paths.map(path => cave :: path)

      // Terminating condition
      if (cave == endCave) newPaths
      else {
        // Where to go next
        val destinations = routesLeft.get(cave)
        // Remove a small cave from the routes
        val newRoutes = if (cave.isSmall) removeCave(routesLeft, cave) else routesLeft

        // No where left to go => dead end
        if (destinations.isEmpty) Nil
        else {
          // recursively add each destination to each existing path
          destinations.get.foldLeft(List[List[Cave]]())((acc, cave) => {
            acc ::: addCaveToPath(newPaths, cave, newRoutes)
          })
        }
      }
    }

    // Start with an empty list and determine the paths
    addCaveToPath(List(List()), startCave, routes)
  }

  def orderPaths(paths: List[List[Cave]]): List[List[Cave]] = {
    // Since the next destinations are prepended, the paths must be reversed
    val ordered = paths.map(list => list.reverse)
    println("Found paths" + ordered.map(l => printPath(l)))
    ordered
  }

  def printPath(path: List[Cave]): String = {
    path.map(c => c.name).mkString(",")
  }

  val lines = readInput(getClass.getResource("input").getFile)
  val routes = pathMap(lines)
  val paths = orderPaths(getAllPaths(routes))

  println("Total number of paths: " + paths.size)
}
