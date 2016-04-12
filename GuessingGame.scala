// The Outguessing Game!! Try to outguess the computer!
// Work in progress - need to make the PlayerGuessProfile more functional style.

// Gets the players guess
def playerGuess(): Char = {
  try{
    return parsePlayerInput(readChar())
  }catch{
    case e: Exception => println("You must input a character"); playerGuess()
  }
}

//Gets the computers guess
def computerGuess(gameRound: Int, gameHistory: String,
   playersLastGuess: Char, playerProfile: PlayerGuessProfile): Char = {
  if (gameRound < 2) {
    randomLorR()
  }else{
    computerGuessLogic(playerProfile, gameHistory, playersLastGuess) match {
      case 'c' => if (playersLastGuess == 'L') 'R' else 'L'
      case 's' => if (playersLastGuess == 'L') 'L' else 'R'
      case 'r' => randomLorR()
    }
  }
}

// Formats the player input to work with the game logic
def parsePlayerInput(input: Char): Char = input match {
  case 'L'|'R'|'l'|'r' => return input.toUpper
  case 'Q'|'q' => System.exit(0); return 'X' // Ugly
  case _ => println("Invalid input - must be L or R"); playerGuess()
}

// Self explanatory
def didPlayerWin(playersGuess: Char, computersGuess: Char): Boolean = {
  if (playersGuess == computersGuess) false else true
}

// Checks if the player changed their choice or stuck with their old choice
def changeOrStick(playersGuess: Char, playersLastGuess: Char): Char = {
  if (playersGuess == playersLastGuess) 's' else 'c'
}

// The brain of the computer player. Checks for player patterns to exploit.
def computerGuessLogic(playerProfile: PlayerGuessProfile,
  gameHistory: String, playersLastGuess: Char): Char = gameHistory match {
    case "wcw" => playerPattern(playerProfile.wcw)
    case "wsw" => playerPattern(playerProfile.wsw)
    case "wcl" => playerPattern(playerProfile.wcl)
    case "wsl" => playerPattern(playerProfile.wsl)
    case "lcw" => playerPattern(playerProfile.lcw)
    case "lsw" => playerPattern(playerProfile.lsw)
    case "lcl" => playerPattern(playerProfile.lcl)
    case "lsl" => playerPattern(playerProfile.lsl)
  }

  // Checks if players are likely to stick or change, if neither returns r
  def playerPattern(playersPattern: String): Char = playersPattern match {
    case "cc" => 'c'
    case "ss" => 's'
    case _ => 'r'
  }

  // Randomly chooses an L or an R
  def randomLorR(): Char = {
    val r = scala.util.Random
    if (r.nextInt(100) > 50) 'L' else 'R'
  }

  // Updates the game history for the pattern matching
  def updateHistory(history: String, lastGuess: Char, newGuess: Char,
    winOrLose: Char): String = {
      val elementOne = history.reverse.dropRight(2)
      val elementTwo = changeOrStick(newGuess, lastGuess)
      val elementThree = winOrLose

      return elementOne + elementTwo + elementThree
    }


// The main game loop, runs the game
def gameLoop(gameRound: Int, playerScore: Int, computerScore: Int,
  playerLastGuess: Char, history: String,
  guessProfile: PlayerGuessProfile): Unit = {

  // Prints Instructions
  println("Round " + gameRound + ": ")
  println("Please enter a guess, L or R: ")

  // Gets the players guess and the computers guess
  val playersGuess = playerGuess()
  val computersGuess = computerGuess(gameRound, history,
    playerLastGuess, guessProfile)

  println("You guessed: " + playersGuess)
  println("Computer guessed: " + computersGuess)

  // Ugly, non-functional, updates state of guessProfile
  guessProfile.updatePlayerProfile(history, playersGuess, playerLastGuess)

  // If the player wins...
  if (didPlayerWin(playersGuess, computersGuess)){
    println("You won!")
    println("Score: You - " + (playerScore+1) + "  Computer - " + computerScore)
    val newHistory = updateHistory(history, playerLastGuess, playersGuess,
      'w')
    gameLoop(gameRound+1,playerScore+1,computerScore,playersGuess,newHistory,
      guessProfile)

  // If the Computer wins...
  }else{
    println("The computer won...")
    println("Score: You - " + playerScore + "  Computer - " + (computerScore+1))
    val newHistory = updateHistory(history, playerLastGuess, playersGuess,
      'l')
    gameLoop(gameRound+1,playerScore,computerScore+1,playersGuess, newHistory,
      guessProfile)
  }
}

// Stores what the player did in past situations to learn from and exploit
class PlayerGuessProfile() {
  var wcw: String = "00"
  var wsw: String = "00"
  var wcl: String = "00"
  var wsl: String = "00"
  var lcw: String = "00"
  var lsw: String = "00"
  var lcl: String = "00"
  var lsl: String = "00"

  // Updates the player profile to reflect current playstyle
  def updatePlayerProfile(history: String, playerGuess: Char,
    playerLastGuess: Char): Unit = history match {
      case "wcw" => wcw = wcw.reverse.dropRight(1) + changeOrStick(playerGuess,
        playerLastGuess)
      case "wsw" => wsw = wsw.reverse.dropRight(1) + changeOrStick(playerGuess,
        playerLastGuess)
      case "wcl" => wcl = wcl.reverse.dropRight(1) + changeOrStick(playerGuess,
        playerLastGuess)
      case "wsl" => wsl = wsl.reverse.dropRight(1) + changeOrStick(playerGuess,
        playerLastGuess)
      case "lcw" => lcw = lcw.reverse.dropRight(1) + changeOrStick(playerGuess,
        playerLastGuess)
      case "lsw" => lsw = lsw.reverse.dropRight(1) + changeOrStick(playerGuess,
        playerLastGuess)
      case "lcl" => lcl = lcl.reverse.dropRight(1) + changeOrStick(playerGuess,
        playerLastGuess)
      case "lsl" => lsl = lsl.reverse.dropRight(1) + changeOrStick(playerGuess,
        playerLastGuess)
  }
}

// Tests
println("Welcome to the OUT-GUESSING GAME...")
println("Guess differently from the computer to score!")
println("The computer scores if your guesses are the same!")
println("Type the letter 'q' to quit!")

// val profile = new PlayerGuessProfile()
// profile.updatePlayerProfile("wcw",'L','L')
// println(profile.wcw)
// profile.updatePlayerProfile("wcw",'L','L')
// println(profile.wcw)
// profile.updatePlayerProfile("wcw",'L','R')
// println(profile.wcw)

val profile = new PlayerGuessProfile()
gameLoop(1,0,0,'s',"lsl", profile)
