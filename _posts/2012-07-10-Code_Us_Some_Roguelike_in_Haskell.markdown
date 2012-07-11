---
layout: post
title: Code Us Some Roguelike in Haskell!
description: A fun game project in Haskell.
---

<h2 class="post_title">Code Us Some Roguelike in Haskell!</h2>

I've recently been inspired by the great live coding of 
[Sokoban](http://www.youtube.com/watch?v=mtvoOIsN-GU&feature=youtu.be) to put
together a console and gui-based [roguelike](http://en.wikipedia.org/wiki/Roguelike)
in Haskell. There are already some really awesome roguelikes like 
[LambdaHack](https://github.com/kosmikus/LambdaHack), 
[Mazes of Monad](http://hackage.haskell.org/package/MazesOfMonad), 
and [Roguestar](http://roguestar.downstairspeople.org/) (I am sorry if I have forgotten any), 
but I want to implement my own. We'll call this little game 'Thieflike.'

I am assuming basic knowledge of Haskell - I'd say through the Monads chapters 
in either [LYAH](http://learnyouahaskell.com/) or 
[RWH](http://book.realworldhaskell.org/). I may also suggest reading the Monad 
Transformers chapter in RWH, although we might skip on using a transformer - 
more about that later.

You need `ansi-terminal` for this project, which you should be able to grab
with `cabal install ansi-terminal`.

To start this project let's just get a player character on the screen
and get it to move around. We'll need something to represent the game's
world as well as the hero. For the hero all we need is a tuple of `Int`s to
represent the position, and the world will be a datatype containing
the hero. Let's start a new file named `Main.hs`, which we'll use to contain
all of our code for this portion of the project.

{% highlight haskell %}
--file: Main.hs
module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

type Coord = (Int, Int)

data World = World { wHero :: Coord }
{% endhighlight %}

Now let's put together a main function and show the character,
represented with the traditional '@' symbol.

{% highlight haskell %}
main = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStrLn "@"
{% endhighlight %}

The `setSGR` function sets the ANSI Select Graphic Rendition mode, essentially
it allows us to do things like set the console foreground and/or
background color, the intensity of the color, etc. See the
[haddock](http://hackage.haskell.org/packages/archive/ansi-terminal/0.5.5/doc/html/System-Console-ANSI.html) for more info.

This is cool that we've started using terminal function, but it doesn't
really do anything. We need to handle input and redraw the screen, so let's
add a data type declaration representing current possible inputs, such as
moving the hero around or exiting the game.

{% highlight haskell %}
data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq)
{% endhighlight %}


Remember when we hid Either in the Prelude module at the beginning of our
file? That's because our possible inputs of `Left` and `Right` conflicts with
`Either`'s data constructors.

Now to write the main loop - we need to handle input and redraw the
screen. `main` must also be rewritten to accommodate changes. 
We also need to add a few lines to make sure that input and output 
is not buffered - so the program will accept input without needing the 
user to press enter, and so that input is not echoed back out.

{% highlight haskell %}
main = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Thieflike"
  gameLoop $ World (0, 0)


gameLoop world@(World hero) = do
  drawHero hero
  input <- getInput
  case input of
    Exit -> return ()
    _    -> handleDir world input


drawHero (heroX, heroY) = do
  clearScreen
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"

-- receive a character and return our Input data structure,
-- recursing on invalid input
getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'w' -> return Up
    's' -> return Down
    'a' -> return Left
    'd' -> return Right
    _ -> getInput

-- given a world and a direction, 'adjust' the hero's position, and loop
-- with our updated hero
handleDir w@(World (heroX, heroY)) input = gameLoop $ w { wHero = newCoord }
  where newCoord = case input of
                    Up    -> (heroX, heroY - 1)
                    Down  -> (heroX, heroY + 1)
                    Left  -> (heroX - 1, heroY)
                    Right -> (heroX + 1, heroY)
{% endhighlight %}

Now the hero moves around using the good old-fashioned 'wasd', and the player
may quit by pressing 'q'.
This mostly does what we want - but what if we start pressing
'a' or 'w' from the starting position? It may appear that
the hero is moving in the opposite direction from where
we want. To keep this from happening we need to limit the
hero's movement. In fact, let's keep the hero in between 0 and
80 in all directions. We should also remove the case section out of
`handleDir`, that way we can apply directions to anything, not just the
player.

{% highlight haskell %}
-- operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


dirToCoord d
  | d == Up    = (0, -1)
  | d == Down  = (0,  1)
  | d == Left  = (-1, 0)
  | d == Right = (1,  0)
  | otherwise  = (0,  0)


-- add the supplied direction to the hero's position, and set that
-- to be the hero's new position, making sure to limit the hero's
-- position between 0 and 80 in either direction
handleDir w@(World hero) input = gameLoop (w { wHero = newCoord })
  where newCoord       = (newX, newY)
        (heroX, heroY) = hero |+| dirToCoord input
        hConst i       = max 0 (min i 80)
        newX           = hConst heroX
        newY           = hConst heroY
{% endhighlight %}

Finally, let's make sure we say goodbye to the player when they press 'q',  
as well as set the cursor back to being visible:

{% highlight haskell %}
-- update the game loop to add in the goodbye message
gameLoop world@(World hero) = do
  drawHero hero
  input <- getInput
  case input of
    Exit -> handleExit
    _    -> handleDir world input



-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"

{% endhighlight %}

Great, the hero moves around and now we exit a little better than better and we
have a goodbye message for the player. This is a small start to our roguelike, but its
a good foundation. Next post we'll work with adding some actual game elements to Thieflike.

The code and an install-able project for this post is located at:
[https://github.com/jamiltron/Thieflike/tree/post01](https://github.com/jamiltron/Thieflike/tree/post01)

Thank you for reading!