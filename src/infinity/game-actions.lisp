(in-package :Tootsville)

;;; src/infinity/game-actions.lisp is part of Tootsville
;;;
;;; Copyright ©  2008-2017, Bruce-Robert  Pocock; Copyright  © 2009,2010
;;; Res  Interactive LLC;  Copyright  © 2018-2021,  the Corporation  for
;;; Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;; This program is Free Software: you can redistribute it and/or modify
;;; it  under the  terms of  the GNU  Affero General  Public License  as
;;; published by the  Free Software Foundation; either version  3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
;;; Affero General Public License for more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA



(defun game-action-start-sports-ball-game (action)
  "Start a SportsBall game.

@gaindex startSportsBallGame

@subsection Usage

A @code{gameAction} packet of the form:

@verbatim
{ c: \"gameAction\",
  d: { action: \"startSportsBallGame\",
       game: UUID } }
@end verbatim

@subsection Example

@verbatim
{ c: \"gameAction\",
  d: { action: \"startSportsBallGame\", 
       game: \"AEB967CB-5598-40D5-9B4A-894C9BC38501\" } }
@end verbatim

@subsection Effects

Sending startSportsBallGame initiates a soccer or other game based on
the same basic premise. The score board is initialized to 0 vs. 0
points, and teams spot one another's goals with
`GAME-ACTION-SPORTS-BALL-GOAL' to increment the score. Optionally, a
timer can be started with startSportsBallTimer, see
`GAME-ACTION-START-SPORTS-BALL-TIMER',
`GAME-ACTION-PAUSE-SPORTS-BALL-TIMER',
`GAME-ACTION-RESUME-SPORTS-BALL-TIMER'.

@cindex SportsBall

@subsection About SportsBall

SportsBall is a game system that is able to roughly support a
football (soccer) match, but does not actively enforce many rules.

In other words, players could choose to play football (soccer) in the
game, but they could also ``cheat'' or ignore rules such as ``off
sides'' that are inconvenient to implement.

The basic principle is that the ball is moving across the field, and
should be returned to the sidelines when it goes out of bounds. The
players attempt to move the ball into one of two goals. When the game
field is dedicated to the game, these goal objects must be designated.

Players can click on the ball to run towards it and kick
it. Collisions between the ball and players will bounce the ball
through the field.
"
  (error 'unimplemented))

(defun game-action-sports-ball-goal (action)
  "Score a goal in a SportsBall game.

@gaindex sportsBallGoal

@subsection Usage

WRITEME

@subsection Example

WRITEME

@subsection Effects

WRITEME

See: `GAME-ACTION-START-SPORTS-BALL-GAME'"
  (error 'unimplemented))

(defun game-action-start-sports-ball-timer (action)
  "Start a timer for a SportsBall game.

@gaindex startSportsBallTimer

@subsection Usage

WRITEME

@subsection Example

WRITEME

@subsection Effects

WRITEME


See: `GAME-ACTION-START-SPORTS-BALL-GAME'"
  (error 'unimplemented))

(defun game-action-pause-sports-ball-timer (action)
  "Pause the timer for a SportsBall game.

@gaindex pauseSportsBallTimer

@subsection Usage

WRITEME

@subsection Effects

WRITEME

See: `GAME-ACTION-START-SPORTS-BALL-GAME', `GAME-ACTION-START-SPORTS-BALL-TIMER'"
  (error 'unimplemented))

(defun game-action-start-sports-ball-timer (action)
  "Resume the timer for a SportsBall game after it had been paused.

@gaindex startSportsBallTimer

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See: `GAME-ACTION-START-SPORTS-BALL-GAME', `GAME-ACTION-START-SPORTS-BALL-TIMER'"
  (error 'unimplemented))



(defun game-action-join-card-game (action)
  "Start playing a card game.

@gaindex joinCardGame

@subsection Usage

@verbatim
{ c: \"gameAction\",
  d: { action: \"joinCardGame\",
       playerP: [ true | false ],
       cardTable: UUID } }
@end verbatim

@cindex Card Games

@subsection Overview of Card Games

Playing cards on a card table uses a special camera view to show the
table top, and a pop-over layer to show the cards in the local
player's hand.

Up to 4 players can join the game. Each player gets a side of the card
table. See also `GAME-ACTION-PART-CARD-GAME'.

The deck of cards can be 52 cards or have the 2 jokers for 54 in
total. See `GAME-ACTION-CARD-GAME-SHUFFLE'.

Players can draw from the deck (`GAME-ACTION-CARD-GAME-DRAW'), deal
cards to other players (`GAME-ACTION-CARD-GAME-DEAL'), and place cards
on the table face-up or face-down (`GAME-ACTION-CARD-GAME-PLAY'), pick
up cards from the table (`GAME-ACTION-CARD-GAME-TAKE'), or move cards
around on the table (`GAME-ACTION-CARD-GAME-MOVE') or in your
hand (`GAME-ACTION-CARD-GAME-ARRANGE').

There are no particular rules of any card game enforced. Players are
free to do whatever they like with the cards.

@subsection Joining a Card Game

The ACTION passed references a card table. If that card table already
has 4 players, this player can only become an observer. Otherwise, the
player can choose to play or to observe.

The structure of ACTION includes these keys:

@table @code

@item cardTable
The UUID of a card table.

@item playerP
If true, this Toot wants to be a player. If false, this Toot wants to
be an observer.

@end table

Attempting to join a card game as a fifth player will result in an
error.

@verbatim
{ from: \"gameAction\", action: \"joinCardGame\", status: false,
  error: USER-ERROR-MESSAGE }
@end verbatim

The user error message will be something suitable for display to the
user to explain why they were refused joining the game.

On success, the player receives a datagram such as

@verbatim
{ from: \"gameAction\", action: \"joinCardGame\", status: true }
@end verbatim

The player is then able to issue other gameAction packets as described
in the preceding overview.

@subsection Usage

WRITEME

@subsection Effects

WRITEME
"
  (error 'unimplemented))

(defun game-action-part-card-game (action)
  "Quit a card game at a card table.

@gaindex partCardGame

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-JOIN-CARD-GAME' for an overview of card table games."
  (error 'unimplemented))

(defun game-action-card-game-shuffle (action)
  "Shuffle all cards into the deck.

@gaindex cardGameShuffle

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-JOIN-CARD-GAME' for an overview of card table games."
  (error 'unimplemented))

(defun game-action-card-game-deal (action)
  "Deal a card from the shuffled deck to another player without looking.

@gaindex cardGameDeal

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-JOIN-CARD-GAME' for an overview of card table games."
  (error 'unimplemented))

(defun game-action-card-game-draw (action)
  "Draw a card from the shuffled deck into your hand.

@gaindex cardGameDraw

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-JOIN-CARD-GAME' for an overview of card table games."
  (error 'unimplemented))

(defun game-action-card-game-play (action)
  "Play a card from your hand, placing it on the table.

@gaindex cardGamePlay

The card may be played face-down or face-up, and at any place on the
table.

@subsection Usage

WRITEME

@subsection Effects

WRITEME

WRITEME

See `GAME-ACTION-JOIN-CARD-GAME' for an overview of card table games."
  (error 'unimplemented))

(defun game-action-card-game-take (action)
  "Pick up a card from the table, placing it into your hand.

@gaindex cardGameTake

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-JOIN-CARD-GAME' for an overview of card table games."
  (error 'unimplemented))

(defun game-action-card-game-move (action)
  "Move a card around on the table.

@gaindex cardGameMove

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-JOIN-CARD-GAME' for an overview of card table games."
  (error 'unimplemented))

(defun game-action-card-game-arrange (action)
  "Re-order the cards in your hand.

@gaindex cardGameArrange

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-JOIN-CARD-GAME' for an overview of card table games."
  (error 'unimplemented))



(defun game-action-start-bowling (action)
  "Start a bowling game.

@gaindex startBowling

This action takes a bowling lane as an argument. The lane is reset and
the scoreboard is wiped clear.

@subsection Usage

WRITEME

@subsection Effects

WRITEME

@cindex Bowling

@subsection Overview of Bowling

WRITEME

@subsection Bowling gameAction actions

@itemize

@item
`GAME-ACTION-BOWLING-RESET-PINS'

@item
`GAME-ACTION-BOWLING-STRIKE-PINS'

@item
`GAME-ACTION-JOIN-BOWLING-GAME'

@item
`GAME-ACTION-PART-BOWLING-GAME'

@item
`GAME-ACTION-GET-BOWLING-SCORECARD'

@end itemize

@subsection Starting a Bowling Game


WRITEME"
  (error 'unimplemented))

(defun game-action-bowling-reset-pins (action)
  "Reset the pins and move to the next player or frame of bowling.

@gaindex resetBowlingPins

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-START-BOWLING' for an overview of bowling."
  (error 'unimplemented))

(defun game-action-bowling-strike-pins (action)
  "Record the bowling ball striking the pins

@gaindex bowlingStrikePins

@subsection Usage

WRITEME

@subsection Effects

WRITEME WRITEME

See `GAME-ACTION-START-BOWLING' for an overview of bowling."
  (error 'unimplemented))

(defun game-action-join-bowling-game (action)
  "Join a bowling game that's about to start

@gaindex joinBowlingGame

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-START-BOWLING' for an overview of bowling."
  (error 'unimplemented))

(defun game-action-part-bowling-game (action)
  "Quit a bowling game that's about to start or already started.

@gaindex partBowlingGame

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-START-BOWLING' for an overview of bowling."
  (error 'unimplemented))

(defun game-action-get-bowling-scorecard (action)
  "Get the scorecard for a bowling game in progress.

@gaindex getBowlingScorecard

@subsection Usage

WRITEME

@subsection Effects

WRITEME
WRITEME

See `GAME-ACTION-START-BOWLING' for an overview of bowling."
  (error 'unimplemented))



(defun game-action-tag-you-re-it (action)
  "@subsection Usage

@gaindex tagYouReIt

WRITEME

@subsection Effects

WRITEME
WRITEME"
  (error 'unimplemented))
