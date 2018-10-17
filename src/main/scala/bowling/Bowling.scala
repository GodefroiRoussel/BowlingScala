// Exercise
// https://github.com/xbucchiotty/kata_bowling_scala


// Test are working but not the main
package bowling

import scala.util.Random

case class Frame(score1: Int = 0, score2: Int = 0) {
    def score: Int = {
        this.score1 + this.score2
    }

    def isStrike: Boolean = {
        this.score1 == 10
    }

    def isSpare: Boolean = {
        this.score1 != 10 && score == 10
    }
}

case class Game(frames: List[Frame], currentFrame: Frame, currentRole: Int) {
    /**
      * Function that roll for a game
      * @param i: Int: The number of touched pins
      * @return the game updated
      */
    def roll(i : Int): Game = {
        if (currentRole == 0){
            Game(frames, Frame(i,0), currentRole + 1)
        } else {
            Game(frames :+ currentFrame.copy(score2 = i), Frame(), 0)
        }
    }

    /**
      * Function that calculate the score of a bowling game, according to spares, strikes and every other score
      * @param currentScore: Int
      * @param frames: List[Frame]
      * @return the score of the game
      */
    def score(currentScore: Int, frames: List[Frame]): Int = {
        if (frames.isEmpty) currentScore
        else {
            val frame: Frame = frames.head

            // Logic for a SPARE
            if(frame.isSpare){

                // If spare look if next frame has been played
                if(frames.tail.nonEmpty){
                    val nextFrame: Frame = frames.tail.head
                    score(currentScore + frame.score + nextFrame.score1, frames.tail)
                } else score(currentScore + frame.score, frames.tail)

            // Logic for a STRIKE
            } else if (frame.isStrike){
                // If spare look if next frame has been played
                if(frames.tail.nonEmpty){
                    val nextFrame: Frame = frames.tail.head
                    // Check if it's a strike to know if we have to consider the next frame
                    if (nextFrame.isStrike){
                        if(frames.tail.tail.nonEmpty){
                            val secondFrameAfterStrike: Frame = frames.tail.tail.head
                            score(currentScore + frame.score + nextFrame.score + secondFrameAfterStrike.score, frames.tail)
                        } else score(currentScore, frames.tail)
                    } else score(currentScore + frame.score + nextFrame.score, frames.tail)
                } else score(currentScore, frames.tail)

            // Logic for if nothing after
            } else
                score(currentScore + frame.score, frames.tail)

        }
    }
}

object Bowling {

    val NB_PINS: Int = 10
    val r: Random = new Random()

    val scoreGame: Int = playGame(r, 10, 0)

    /**
      * Function that return the score after a frame
      * @param nbPins
      * @param nbPinsDown
      * @return
      */
    def scoreFrame(nbPins: Int, nbPinsDown: Int): Int = {
        nbPins - nbPinsDown
    }

    /**
      * Function to play the game
      * @param r: Random
      * @param nbFrameToPlay: Int
      * @param score: Int
      * @return the score of the game
      */
    def playGame(r: Random, nbFrameToPlay: Int, score: Int): Int = {
        val newFrame: Frame = playFrame(r)
        playGame(r, nbFrameToPlay-1, score + newFrame.score)
    }

    /**
      * Funtion to play a frame
      * @param r: Random
      * @return the score of the frame
      */
    def playFrame(r: Random): Frame = {
        val firstRoll: Int = roll(r, NB_PINS)

        if (firstRoll == 10) {
            val secondRoll: Int = roll(r, NB_PINS-firstRoll)
            Frame(firstRoll, secondRoll)
        } else {
            Frame(10, 0)
        }
    }

    /**
      * Function that return a random number of pins downed
      * @param r: Random
      * @param nbPins: Int
      * @return a
      */
    def roll(r: Random, nbPins: Int) : Int = {
        r.nextInt(nbPins)+1
    }
}