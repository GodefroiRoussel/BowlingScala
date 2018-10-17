package bowling

import org.scalatest.{FunSpec, Matchers}

class BowlingSpec extends FunSpec with Matchers {
    val g: Game = Game(List(), Frame(), 0)

    /////////////////////// A ROLL ///////////////////////////////////
    describe("A roll") {
        describe("with 0 pin") {
            val gameUpdated: Game = g.roll(0)
            val frame: Frame = gameUpdated.currentFrame
            it("should give a 0 score") {
                assert(frame.score == 0)
            }
        }

        describe("with 1 pin") {
            val gameUpdated: Game = g.roll(1)
            val frame: Frame = gameUpdated.currentFrame
            it("should give a 1 score") {
                assert(frame.score == 1)
            }
        }

        describe("with 10 pin") {
            val gameUpdated: Game = g.roll(10)
            val frame: Frame = gameUpdated.currentFrame
            it("should give a strike") {
                assert(frame.score == 10)
            }
        }
    }

    ////////////////////////// A FRAME /////////////////////////////

    describe("A frame") {
        describe("with 0 pin for each roll") {
            val gameUpdated1: Game = g.roll(0)
            val gameUpdated2: Game = gameUpdated1.roll(0)
            val frame: Frame = gameUpdated2.frames.head
            it("should give a 0 score") {
                assert( frame.score == 0)
            }
        }

        describe("with 1 pin for each roll") {
            val gameUpdated1: Game = g.roll(1)
            val gameUpdated2: Game = gameUpdated1.roll(1)
            val frame: Frame = gameUpdated2.frames.head
            it("should give a 1 score") {
                assert(frame.score == 2)
            }
        }

        describe("with 5 pins for each roll. That give a spare!") {
            val gameUpdated1: Game = g.roll(5)
            val gameUpdated2: Game = gameUpdated1.roll(5)
            val frame: Frame = gameUpdated2.frames.head
            it("should give a spare") {
                assert(frame.score == 10)
            }
        }
    }

    /////////////////////////// ALL ROLL ////////////////////////////////

    describe("All roll") {
        describe("with 0 pin") {

            val game: Game = allGame(g, 0, 0, 10)
            it("should give a 0 score") {
                assert(game.score(0, game.frames) == 0)
            }
        }

        describe("with 1 pin") {
            val game: Game = allGame(g, 1, 1, 10)
            it("should give a 20 score") {
                assert(game.score(0, game.frames) == 20)
            }
        }

        describe("with strike") {
            val game: Game = allGame(g, 10, 0, 10)
            it("should give a 300 score") {
                assert(game.score(0, game.frames) == 240)
            }
        }

        describe("with strike then 2 last rolls with 0") {
            val game: Game = allGame(g, 10, 0, 10)
            val gameAfterLastFrame: Game = gameTurn(game, 0,0)
            it("should give a 270 score") {
                assert(gameAfterLastFrame.score(0, gameAfterLastFrame.frames) == 270)
            }
        }

        describe("with strike and add the 2 lasts after the 10th") {
            val game: Game = allGame(g, 10, 0, 12)
            it("should give a 300 score") {
                assert(game.score(0, game.frames) == 300)
            }
        }
    }

    ///////////////////////////////// SPARE //////////////////////////////
    describe("A spare") {
        val game: Game = gameTurn(g, 5,5)
        describe("With nothing after"){
            val gameAfterSpare: Game = gameTurn(game, 0,0)
            it("should give a 10 score") {
                assert( gameAfterSpare.score(0, gameAfterSpare.frames) == 10)
            }
        }

        describe("With a 1 roll after"){
            val gameAfterSpare: Game = gameTurn(game, 1,0)
            it("should give a 12 score") {
                assert(gameAfterSpare.score(0, gameAfterSpare.frames) == 12)
            }
        }
    }

    ////////////////////////////// STRIKE /////////////////////////////////
    describe("A Strike") {
        val game: Game = gameTurn(g, 10,0)
        describe("With nothing after"){
            val gameAfterStrike: Game = gameTurn(game, 0,0)
            it("should give a 10 score") {
                assert( gameAfterStrike.score(0, gameAfterStrike.frames) == 10)
            }
        }

        describe("With a 1 roll after"){
            val gameAfterStrike: Game = gameTurn(game, 1,0)
            it("should give a 14 score") {
                assert(gameAfterStrike.score(0, gameAfterStrike.frames) == 12)
            }
        }

        describe("With a 1 and a 5 roll after"){
            val gameAfterStrike: Game = gameTurn(game, 1,5)
            it("should give a 22 score") {
                assert(gameAfterStrike.score(0, gameAfterStrike.frames) == 22)
            }
        }

        describe("With a strike and a strike after"){
            val gameAfterStrike: Game = gameTurn(game, 10,0)
            val gameAfterDoubleStrike: Game = gameTurn(gameAfterStrike, 10,0)
            it("should give a 30 score") {
                assert(gameAfterDoubleStrike.score(0, gameAfterDoubleStrike.frames) == 30)
            }
        }
    }

    /**
      * Function that plays nbTurnLeft of the game with the same roll1 and roll2 each time
      * @param game: Game
      * @param roll1: Int
      * @param roll2: Int
      * @param nbTurnLeft: Int
      * @return the game at the end
      */
    def allGame(game: Game, roll1: Int, roll2: Int, nbTurnLeft: Int): Game = {
        if (nbTurnLeft == 0) game
        else allGame(gameTurn(game, roll1, roll2), roll1, roll2, nbTurnLeft-1)
    }

    /**
      * Function that plays the turn of a game with the roll1 and roll2 parameter
      * @param game: Game
      * @param roll1: Int
      * @param roll2: Int
      * @return the game updated after a turn
      */
    def gameTurn(game: Game, roll1: Int, roll2: Int): Game = {
        val gameUpdated: Game = game.roll(roll1)
        gameUpdated.roll(roll2)
    }
}
