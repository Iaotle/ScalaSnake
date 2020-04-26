package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game._
import snake.logic.SnakeLogic._

import scala.collection.mutable.ArrayBuffer

/** To implement Snake, complete the ``TODOs`` below.
  *
  * If you need additional files,
  * please also put them in the ``snake`` package.
  */

class SnakeLogic(val randomGen: RandomGenerator, val nrColumns: Int, val nrRows: Int) {

  def this() = this(new ScalaRandomGen(), DefaultRows, DefaultColumns)

  case class Coordinate(x: Int, y: Int) {
    def +(rhs: Coordinate): Coordinate = {
      if      (rhs.x + x < 0) Coordinate(nrColumns - 1, (y + rhs.y) % nrRows) // negative x

      else if (rhs.y + y < 0) Coordinate((x + rhs.x) % nrColumns, nrRows - 1)

      else                    Coordinate((x + rhs.x) % nrColumns, (y + rhs.y) % nrRows) // normal wrap
    }
  }

  case class SnakeClass(var snakeArray: ArrayBuffer[Coordinate], var headLoc: Coordinate, var snakeDir: Direction, var snakeEatHead: Direction, var desiredSize: Int){

    def moveNorth():  Unit =                            snakeArray.append(snakeArray.last + Coordinate(0, -1))
    def moveSouth():  Unit =                            snakeArray.append(snakeArray.last + Coordinate(0, 1))
    def moveEast():   Unit =                            snakeArray.append(snakeArray.last + Coordinate(1, 0))
    def moveWest():   Unit =                            snakeArray.append(snakeArray.last + Coordinate(-1, 0))

    def prepend(coordinate: Coordinate): Unit =         snakeArray.prepend(coordinate)
    def remove(index: Int): Unit =                      snakeArray.remove(index)

    def getBody(): ArrayBuffer[Coordinate] =            snakeArray
    def getCurrentSize(): Int =                         snakeArray.size

    def setHeadLoc(coordinate: Coordinate): Unit =      headLoc = coordinate
    def getHeadLoc(): Coordinate =                      headLoc

    def setDir(direction: Direction): Unit =            if (direction != snakeEatHead) snakeDir = direction
    def getDir(): Direction =                           snakeDir

    def setEatHead(direction: Direction): Unit =        snakeEatHead = direction

    def setSize(size: Int): Unit =                      desiredSize = size
    def getDesiredSize(): Int =                         desiredSize
  }

  case class HistoryClass(var snakeHistory: ArrayBuffer[Coordinate], var dirHistory: ArrayBuffer[Direction], var sizeHistory: ArrayBuffer[Int], var appleCount: ArrayBuffer[Int], var appleArr: ArrayBuffer[Coordinate]){

    def appendHistory(coordinate: Coordinate): Unit =   snakeHistory.append(coordinate)
    def updateHistory(): Unit =                         snakeHistory.remove(snakeHistory.size-1)
    def getHistory(): Coordinate =                      snakeHistory.last

    def appendDir(direction: Direction): Unit =         dirHistory.append(direction)
    def updateDir(): Unit =                             dirHistory.remove(dirHistory.size-1)
    def getDir(): Direction =                           dirHistory.last

    def appendSize(size: Int): Unit =                   sizeHistory.append(size)
    def updateSize(): Unit =                            sizeHistory.remove(sizeHistory.size-1)
    def getSize(): Int =                                sizeHistory.last

    def appendApple(count: Int): Unit =                 appleCount.append(count)
    def updateApple(): Unit =                           appleCount.remove(appleCount.size-1)
    def getApple(): Int =                               appleCount.last

    def appendAppleLoc(coordinate: Coordinate): Unit =  appleArr.append(coordinate)
    def updateAppleLoc(): Unit = appleArr.remove(appleArr.size-1)
    def getAppleLoc(): Coordinate = appleArr.last
  }

  case class AppleClass(var appleLoc: Coordinate){
    def place(coordinate: Coordinate): Unit =           appleLoc = coordinate
    def getLoc(): Coordinate =                          appleLoc
  }

  var count =       0
  var timeTravel =  false
  var snake =       SnakeClass(ArrayBuffer(Coordinate(0,0), Coordinate(1,0), Coordinate(2,0)), Coordinate(2,0), East(), West(), 0)
  var apple =       AppleClass(Coordinate(0,0))
  var history =     HistoryClass(ArrayBuffer(), ArrayBuffer(East()), ArrayBuffer(3), ArrayBuffer(), ArrayBuffer())

  placeApple()

  def step(): Unit = {

    if (timeTravel & count > 0) {
      replaceApple()
      moveReverse()
    }

    else if (!timeTravel & !isGameOver) {
      moveSnake()
      if (eatApple) placeApple()
    }
  }

  def setReverseTime(reverse: Boolean): Unit = timeTravel = reverse

  def isGameOver: Boolean = snake.getBody().slice(0, snake.getCurrentSize()-1).contains(snake.getHeadLoc()) // body contains head

  def eatApple: Boolean = apple.getLoc() == snake.getBody().last

  def moveSnake(): Unit = {
    if (snake.getCurrentSize() == snake.getDesiredSize()) { // if the snake is not growing
      history.appendHistory(snake.getBody()(0))
      snake.remove(0)
    }
    snake.getDir() match{
      case North()  =>  snake.moveNorth()
      case South()  =>  snake.moveSouth()
      case East()   =>  snake.moveEast()
      case West()   =>  snake.moveWest()
    }

    snake.setHeadLoc(snake.getBody().last)
    history.appendSize(snake.getDesiredSize())
    history.appendDir(snake.getDir())
    snake.setEatHead(snake.getDir().opposite)

    count = count + 1
  }

  def placeApple(): Unit = {
    if (getFreeSpace > 0) findPlacement()
    else                  apple.place(Coordinate(nrColumns, nrRows)) // place it out of bounds if there's no space for it

    snake.setSize(snake.getDesiredSize()+3)
  }

  def findPlacement(): Unit = {
    var countdown = randomGen.randomInt(getFreeSpace)
    for (row <- 0 until nrRows; col <- 0 until nrColumns) {
      if (getGridTypeAt(col, row) == Empty()) {
        createApple(countdown, col, row)
        countdown = countdown - 1
      }
    }
  }

  def createApple(countdown: Int, col: Int, row: Int): Unit ={ // I'd have put this in findPlacement() (it's basically just a for loop) but grading guidelines say max 2 levels of control...
    if (countdown == 0) {
      apple.place(Coordinate(col, row))
      history.appendApple(count)
      history.appendAppleLoc(apple.getLoc())
      println("placing apple at ", col, row, "appending ", apple.getLoc())
    }
  }

  def getFreeSpace: Int = nrRows * nrColumns - snake.getCurrentSize()

  def getGridTypeAt(x: Int, y: Int): GridType = {
    if            (apple.getLoc() == Coordinate(x, y))          Apple()
    else if       (snake.getHeadLoc() == Coordinate(x, y))      SnakeHead(history.getDir())
    else if       (snake.getBody().contains(Coordinate(x, y)))  SnakeBody()
    else                                                        Empty()
  }

  def moveReverse(): Unit = {

    count = count - 1

    snake.setDir(history.getDir())
    history.updateDir()

    if (snake.getCurrentSize() == snake.getDesiredSize()) { // if we're not shrinking
      snake.prepend(history.getHistory())
      history.updateHistory()
    }

    snake.remove(snake.getCurrentSize()-1)
    snake.setHeadLoc(snake.getBody().last)

    snake.setSize(history.getSize())
    history.updateSize()
  }

  def replaceApple(): Unit = {
    if (count == history.getApple()) {
      println("replacing apple at ", history.getAppleLoc(), "count = ", history.getApple())
      history.updateAppleLoc()
      history.updateApple()
      apple.place(history.getAppleLoc())

    }
  }

  def changeDir(d: Direction): Unit = {
    snake.setDir(d)
  }
}

/** SnakeLogic companion object */
object SnakeLogic {

  val DefaultColumns = 25
  val DefaultRows = 25

}