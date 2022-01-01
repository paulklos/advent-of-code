package day12

import day08.Day08a.readInput
import day12.Day12a.{orderPaths, pathMap, removeCave}

object Day12b extends App {

  /**
   * Determine all the paths through the cave system that begin in the 'start' cave and lead to the 'end' cave.
   * @param routes the route map
   * @return the paths, represented as a list of caves.
   */
  def getAllPaths(routes: Map[Cave, List[Cave]]): List[List[Cave]] = {
    val startCave = Cave("start")
    val endCave = Cave("end")

    // Add a cave to all paths found so far
    def addCaveToPath(paths: List[List[Cave]], cave: Cave, routesLeft: Map[Cave, List[Cave]], smallCavesVisited: Set[Cave], visitedTwice: Option[Cave]): List[List[Cave]] = {

      // Updated paths, with the cave added
      val newPaths = paths.map(path => cave :: path)

      // Terminating condition
      if (cave == endCave) newPaths
      else {
        // Where to go next
        val destinations = routesLeft.get(cave)

        // No where left to go => dead end
        if (destinations.isEmpty) Nil
        else {

          val newSmallCavesVisited = if (cave.isSmall) smallCavesVisited + cave else smallCavesVisited
          val newRoutes = {
            // Remove all the small caves visited, including the current cave, from the routes if this is the second time we're visiting this cave
            if (smallCavesVisited.contains(cave)) newSmallCavesVisited.foldLeft(routesLeft)((acc, cave) => removeCave(acc, cave))
            // Small cave and some cave was already visited twice => simply remove the cave from the routes immediately, because visiting it again is no allowed
            // If the small cave is the start cave, also remove it immediately
            else if (cave.isSmall && visitedTwice.nonEmpty || cave == startCave) removeCave(routesLeft, cave)
            // No changes to the routes in all other cases
            else routesLeft
          }
          // If this is the cave we're visiting twice, keep track of that
          val newVisitedTwice = if (smallCavesVisited.contains(cave)) Option(cave) else visitedTwice

          // recursively add each destination to each existing path
          destinations.get.foldLeft(List[List[Cave]]())((acc, cave) => {
            acc ::: addCaveToPath(newPaths, cave, newRoutes, newSmallCavesVisited, newVisitedTwice)
          })
        }
      }
    }

    // Start with an empty list and determine the paths
    addCaveToPath(List(List()), startCave, routes, Set(), Option.empty[Cave])
  }

  val lines = readInput(getClass.getResource("input").getFile)
  val routes = pathMap(lines)
  val paths = orderPaths(getAllPaths(routes))

  println("Total number of paths: " + paths.size)
}
