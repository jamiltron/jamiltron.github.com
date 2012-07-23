---
layout: post
title: Code Us Some Roguelike in Haskell (Part 2)!
description: Part 2 of the Roguelike tutorial.
---

<h2 class="post_title">Code Us Some Roguelike in Haskell (Part 2)!</h2>

This is part 2 of a multi-part tutorial:

[Part 1](http://jamiltron.com/2012/07/Code_Us_Some_Roguelike_in_Haskell.html)



### Design Decisions ###

This post is more focused on what sort of a game we are
making. For purposes of this tutorial Thieflike must be
relatively small. The idea of roguelikes may be a little
different for each person, so I want to go over the things
I think needs to be in this game to place it within that
genre:

* Random dungeons
* Monsters
* Gear & Potions
* Treasure
* Doors and the bashing thereof
* Environmental stuff within the dungeon (pits, traps, etc.)
* Line-of-Sight
* Mapping


Even with such a small set of features the game could
get pretty hairy. Especially since we intend to eventually
add support for a graphical front-end beyond the console.
How can we keep each of those things in the game, while
still keeping it simple?


#### Random Dungeons ####

While there are all sorts of awesome ways to randomize
each level, we will be sticking to something like the
traditional method used in Rogue. This may not produce
the snazziest environments around, but it will serve the
purpose and prevent our levels from becoming superfluous
given the number of elements we are playing with.

We'll also be implementing a system for random/wandering
monsters and treasure layout similar to the old D&D Red Box,
where each room has specific random percentages for oddities.

Since we're making our design decision right now we won't be
doing the random dungeon part until next post, this time
I'll be copping out with a pre-created string.


#### Monsters ####

This may be blasphemous - but we'll only have one enemy
type, and its strength will be based entirely around
how deep within the dungeon it is. We can always add more
later, but this will keep complexity down initially.

Let's call this monster type 'villain', just because
I think that's a funny monster type name. You can
call it 'orc', or 'were-gelatinous cube', or whatever you
like.

Last tutorial we kept all of our data type declarations
in Main, but now we're going to have a ton more. For now
let's break those out into a new file called `Types.hs`
in the same directory as `Main.hs`

{% highlight haskell %}
--file: Types.hs
module Types where

import qualified Data.Map as M

type Coord = (Int, Int)

-- foul beasts
data Villain = Villain { vCurrPos :: Coord
                       , vGold    :: Int
                       , vHP      :: Int
                       , vItems   :: [Item]
                       , vOldPos  :: Coord }

{% endhighlight %}

We move `Coord` from `Main.hs` into `Types.hs` and we
import `Data.Map`, because a lot of our data will be
stored in a map. There are many name clashes in a lot of the
`Data.*` libraries, so I always like to qualify the import
as the first letter of the primary data type provided.

`vCurrPos` and `vOldPos` are coordinates to where
the villain is currently, and where they were last turn.
We'll be using this to make sure we only draw positions
on screen that have actually changed. The other attributes are 
obvious monster stuff - how many hit points a creature has left, 
what gold its carrying, and what items it possesses...which leads us to our next point.


#### Gear ####

The only gear that is essential for me is weapons,
potions, and armor. Since we aren't including a leveling
or skill system, the gear a hero finds will kind of
be progress, assuming the deeper the dungeon level is the 
better its gear is.

{% highlight haskell %}
-- file: Types.hs

data Item = Arm Armor
          | Pot Potion
          | Weap Weapon


data Armor = Armor { aDefense :: Int
                   , aDest    :: String }


data Potion = Potion { pAmount :: Int
                     , pDesc   :: String
                     , pEffect :: Effect }


data Effect = Harm
            | Heal


data Weapon = Weapon { wDamage :: Int
                     , wDesc   :: String
                     , wToHit  :: Int }

{% endhighlight %}

Most of the item attributes will become more obvious when
we begin putting together the combat system.


#### Treasure ####

Just like monsters, there will be only one form
of currency laid out throughout the dungeon - gold. It also
won't really do anything other than provide a score system.


#### Environmental Stuff ####

Most of the things that can't be fought/picked-up will be
represented as tiles - stairs, doors, etc. To keep down on
what we need to program we won't be implementing traps, but
we should have at least one damaging environmental hazard - 
let's put in giant pools of acid.

{% highlight haskell %}
-- file: Types.hs

data Tile = Acid
          | Dr   Door
          | St   Stairs
          | Wall 


data Door = Closed
          | Open


data Stairs = Downstairs
            | Upstairs

{% endhighlight %}


#### Line-of-Sight and Mapping ####

We'll implement mapping by keeping a dictionary of
coordinates with a `Bool` of whether or not the space
has been mapped before. Line-of-sight will be discussed in
a later post.

<br />
### Wrapping up Types ###

We still need to move our Hero, Input and World declaration from `Main.hs` to 
`Types.hs`. We also need to set up a data type to represent the level.

{% highlight haskell %}
-- file: Types.hs

data Input = Dir Direction
           | Exit


data Direction = Up
               | Down
               | Left
               | Right


data Hero = Hero { hCurrPos :: Coord   
                 , hGold    :: Int    
                 , hHP      :: Int    
                 , hItems   :: [Item] 
                 , hOldPos  :: Coord  
                 , hWield   :: Weapon 
                 , hWears   :: Armor  }


data Level = Level { lDepth    :: Int                   
                   , lGold     :: M.Map Coord Int  
                   , lItems    :: M.Map Coord Item 
                   , lMapped   :: M.Map Coord Bool
                   , lMax      :: Coord            
                   , lTiles    :: M.Map Coord Tile 
                   , lVillains :: M.Map Coord Villain } 


data World = World { wDepth  :: Int
                   , wHero   :: Hero     
                   , wLevel  :: Level    
                   , wLevels :: [Level] }

{% endhighlight %}

The Hero is pretty straightforward - similar to the villain
except the Hero can wield weapons and wear armor.

Level needs some explaining. Now, I'm not saying
I'm an expert roguelike or game developer and I'm not saying this is
necessarily the best way to represent a world, but it
certainly works. `lDepth` says how deep in the dungeon
this particular level is, and `lMax` is the largest (x,y)
coordinate for the level. Every other attribute is a
mapping from coords to a representative type.

To get the game up and running we need to come up with some defaults
data structures. I'll be cheating a little on the default level and the
hero's position, but we'll fix that when we generate random worlds.

{% highlight haskell %}
-- file: Types.hs

emptyLevel = Level { lDepth    = 0
                   , lGold     = M.empty
                   , lItems    = M.empty
                   , lMapped   = M.fromList [((1,1), True)]
                   , lMax      = (1,1)  
                   , lTiles    = M.empty
                   , lVillains = M.empty }


-- bare fists/no weapon
fists = Weapon 0 "Bare fists" 0


-- no armor
rags = Armor 0 "Rags"


-- a basic world used to start the game
genesis  = World { wDepth  = 0
           , wHero   = commoner  
           , wLevel  = emptyLevel
           , wLevels = [emptyLevel] }  -- all levels


-- a basic hero
commoner = Hero { hCurrPos = (1,1)
                , hGold   = 0  
                , hHP     = 10 
                , hItems  = [] 
                , hOldPos = (1,1)
                , hWeild  = fists
                , hWears  = rags }

{% endhighlight %}
<br />
### Building a Test Level ###

If we're going to build a small test level we're going to
need a couple functions to help out. Open a new file and
call it `Level.hs`. Usual imports here:

{% highlight haskell %}
-- file: Level.hs
module Level where

import qualified Data.Map  as M

import Types

{% endhighlight %}

The initial level will be given to us as a string, so we'll
need to translate characters into our maps.

{% highlight haskell %}
-- file: Level.hs

strsToLevel :: [String] -> Level
strsToLevel str = foldl populate emptyLevel {lMax=maxXY} asciiMap
  where
    asciiMap = concat $ zipWith zip coords str
    coords   = [[(x, y) | x <- [0..]] | y <- [0..]]
    maxX     = maximum . map (fst . fst) $ asciiMap
    maxY     = maximum . map (snd . fst) $ asciiMap
    maxXY    = (maxX, maxY)
    populate lvl (coord, tile) =
      case tile of
        '#'   -> lvl { lTiles = M.insert coord Wall            tiles }
        '>'   -> lvl { lTiles = M.insert coord (St Downstairs) tiles }
        '<'   -> lvl { lTiles = M.insert coord (St Upstairs)   tiles }
        '+'   -> lvl { lTiles = M.insert coord (Dr Closed)     tiles }
        '-'   -> lvl { lTiles = M.insert coord (Dr Open)       tiles }
        '~'   -> lvl { lTiles = M.insert coord Acid            tiles }
        _     -> lvl
        where tiles = lTiles lvl

{% endhighlight %}

Most of this function is shorthand , let's take a look at it piece by piece.

`asciiMap` is a list of tuples with a `Coord` as the first element, and a
character from the string as the second. The types here are important to
understand what's going on. `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`,
`zip :: [a] -> [b] -> [(a, b)]`, `coords` is <i>basically</i> `[[Coord]]`
 and `str :: [String]`. So `zip` is `zipWith`'s `(a -> b -> c)`, `[[Coord]]` is
 `zip`'s `[[a]]`, etc.

`coords` may seem confusing if you're coming from a language that doesn't support
lazy lists. It is in fact a a list of lists, where the first element is a list of
<i>basically</i> `Coords`, all with `y` bound to `0` and `x` bound to `[0..infinity]`.
Each additional element binds `y` to successively greater values. In a nutshell, this 
is building a list of infinite coordinates, where each element represents a row with
an infinite number of columns. Fortunately `zip` tangles our problem of infinity - it only
ties as many elements to each actual row as there are available in `str`. By `zipWith`ing
 `zip` we're able to combine each coord to its corresponding character in char, but we have
one problem left over - they are all one list too deep. Fortunately `concat` solves this for us.
`concat :: [[a]] -> [a]` 'flattens' a list of lists one-level.

If this doesn't make much sense at first I'd suggest playing around with functions like `zip` and
`zipWith`. They are immensely useful, along with functions like the `fold`s and `scan`s for
manipulating sequences functionally and elegantly where one may have thought required some
sort of an iterative loop. In this case its easy to keep `coords` contained in `strsToLevel`
because that's the only place its going to be used, but when crafting your own sequencing
functions if you find them difficult you may want to break them out into their own top-level
functions so you can check the types easier with either GHC or GHCi.

After building up our `asciiMap` we `foldl` an empty level (plus calculated max value based
on the dimensions of `str`) over a function we're calling `populate`. So we take our level,
consume the next character in `asciiMap`, and return a new level for further folding (if there
are any chars left). `populate` looks for specific characters and inserts them into the level
being returned.

Another set of functions we're going to need is determining if a particular `Coord` is one of
our dungeon features or not. Fortunately because our game is so simple we can basically just
set functions like `isGold` to be `Data.Map`'s `member` function for a given level's `lGold`
map. There are a few cases where we do have to perform a `lookup` on our maps, such as with
`Item`s and `Tile`s, because we support multiple types within those corresponding dictionaries.

{% highlight haskell %}
-- file: Level.hs

isAcid coord lvl = case M.lookup coord (lTiles lvl) of
  Just Acid -> True
  _         -> False


isClosedDoor coord lvl = case M.lookup coord (lTiles lvl) of
  Just (Dr Closed) -> True
  _                -> False


isOpenDoor coord lvl = case M.lookup coord (lTiles lvl) of
  Just (Dr Open) -> True
  _              -> False


isWall coord lvl = case M.lookup coord (lTiles lvl) of
  Just Wall -> True
  _         -> False


isDownstairs coord lvl = case M.lookup coord (lTiles lvl) of
  Just (St Downstairs) -> True
  _                    -> False


isUpstairs coord lvl = case M.lookup coord (lTiles lvl) of
  Just (St Upstairs) -> True
  _                  -> False


isGold coord lvl = M.member coord (lGold lvl)


isVillain coord lvl = M.member coord (lVillains lvl)


isArmor coord lvl = case M.lookup coord (lItems lvl) of
  Just (Arm _) -> True
  _            -> False


isPotion coord lvl = case M.lookup coord (lItems lvl) of
  Just (Pot _) -> True
  _            -> False


isWeapon coord lvl = case M.lookup coord (lItems lvl) of
  Just (Weap _) -> True
  _             -> False


{% endhighlight %}


And finally finishing up level for now we'll construct a 'cheater'
level until we get the random dungeon generator up and running.

{% highlight haskell %}
-- file: Level.hs

map1   = [ "##############"
         , "#>           #          ######"
         , "#            ############    #"
         , "#            -          +    #"
         , "#    ~~      ############    #"
         , "#     ~~     #          #    #"
         , "#      ~~    #          # <  #"
         , "##############          ######" ]
            

level1 = strsToLevel map1

{% endhighlight %}

I've never quite been a fan of how hallways are typically represented in roguelikes,
so they'll just look like really long rooms.

<br />
### Graphics ###

While we're handling breaking level out into its own file, let's break out all of the
drawing into one file. We'll call it `Console.hs`, because we eventually want to make
a GUI front-end as well.


{% highlight haskell %}
-- file: Console.hs

module Console where

import System.Console.ANSI

import Level
import Types


coordToChar coord (World _ hero lvl _)
  | hCurrPos hero == coord      = '@'
  | isAcid        coord lvl     = '~'
  | isClosedDoor  coord lvl     = '+'
  | isOpenDoor    coord lvl     = '-'
  | isDownstairs  coord lvl     = '<'
  | isGold        coord lvl     = '$'
  | isPotion      coord lvl     = '!'
  | isUpstairs    coord lvl     = '>'
  | isVillain     coord lvl     = 'v'
  | isWall        coord lvl     = '#'
  | isWeapon      coord lvl     = ')'
  | otherwise                   = ' '


drawChar '@' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putChar '@'
drawChar '#' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ]
  putChar  '#'
drawChar '!' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Magenta]
  putChar '!'
drawChar '$' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Yellow ]
  putChar '$'
drawChar 'v' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red ]
  putChar 'v'
drawChar ')' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Cyan ]
  putChar ')'
drawChar '>' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Dull Blue ]
  putChar '>'
drawChar '<' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Dull Cyan ]
  putChar '<'
drawChar '\n' = do
  putChar '\n'
drawChar '+' = do
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Dull Magenta ]
  putChar '+'
drawChar '-' = do
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Dull Yellow ]
  putChar '-'
drawChar '~' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Green ]
  putChar '~'  
drawChar _ = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ]
  putChar ' '

{% endhighlight %}

Those functions should be pretty straight forward, go ahead and change the
colors and intensities to whatever you wish. Now let's remove the various
draw functions from `Main.hs` and make new versions of them.

{% highlight haskell %}
-- file: Console.hs

drawCoord world coord = do
  uncurry (flip setCursorPosition) coord
  drawChar (coordToChar coord world) 
  
  
drawHero world
  | newPos == oldPos = return ()
  | otherwise        = do
    drawCoord world newPos
    drawCoord world oldPos
  where
    hero   = wHero world
    newPos = hCurrPos hero
    oldPos = hOldPos  hero
  

drawWorld world = do
  setCursorPosition 0 0
  mapM_ drawChar (unlines chars)
  where
    lvl     = wLevel world
    (x',y') = lMax lvl
    chars   = [[coordToChar (x,y) world | x <- [0..x']]
                                        | y <- [0..y']]

{% endhighlight %}

`drawCoord` does something a little funky - I should have
mentioned in my first post that while our `Coord`s are `(x, y)`,
 `setCursorPosition` is looking for input in the style of `y -> x`.
 So we `flip` that function to get it to accept `x` first, and then we
`uncurry` it. That transforms it from being a normal curried function
into being a function that will accept our `Coord`.

`drawHero` is pretty obvious, we just make sure to redraw the coord
that the hero was previously on, and then draw the position of the
hero as it is currently. This will keep our draws down.

`drawWorld` is only intended to be drawn every so often. It sets the cursor
to the top-left of the screen, and then it draws all the characters from left to
right, top to bottom. We use the same infinite list of list of coords that we did
in `strsToLevel`, and we `unlines` it to intersperse newlines inbetween each row
so that we don't get one long line of every character on the map.

That does it for `Console.hs`, let's get `Main.hs` done and get to playing. Or rather,
get to moving a guy around a map, but at least we got most of the foundation for the game
pretty much down!

<br />
### Main ###

Start out by importing everything we've done so far, and let's adjust our
`main` function to account for our new data types.

{% highlight haskell %}
-- file: Main.hs
module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

import Console
import Level
import Types

main = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Thieflike"
  clearScreen
  let world = genesis { wLevel = level1, wLevels = [level1] }
  drawWorld world
  gameLoop world

{% endhighlight %}

We only need to draw the whole world once on this iteration of Thieflike, so
we do that right before jumping into the gameLoop.

{% highlight haskell %}
-- file: Main.hs

gameLoop world = do
  drawHero world
  input <- getInput
  case input of
    Exit    -> handleExit
    Dir dir -> handleDir world dir


getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'w' -> return (Dir Up)
    's' -> return (Dir Down)
    'a' -> return (Dir Left)
    'd' -> return (Dir Right)
    _   -> getInput

{% endhighlight %}

`gameLoop` and `getInput` are similar to before, but we account for the fact that 
`Input` is either `Dir Direction` or simply `Exit`. We also make sure to draw the 
hero prior to receiving input.

{% highlight haskell %}
-- file: Main.hs

handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [Reset]
  putStrLn "Thank you for playing!"

{% endhighlight %}

I was pointed out in comments to [post 1](http://jamiltron.com/2012/07/Code_Us_Some_Roguelike_in_Haskell.html)
that I should be calling `setSGR [Reset]` prior to exiting.

{% highlight haskell %}
-- file: Main.hs

dirToCoord Up    = (0, -1)
dirToCoord Down  = (0,  1)
dirToCoord Left  = (-1, 0)
dirToCoord Right = (1,  0)


handleDir w dir
  | isWall coord lvl ||
    isClosedDoor coord lvl = gameLoop w { wHero = h { hOldPos = hCurrPos h } }
  | otherwise              = gameLoop w { wHero = h { hOldPos  = hCurrPos h
                                                    , hCurrPos = coord } }
  where 
    h              = wHero w
    lvl            = wLevel w
    coord          = (newX, newY)
    newX           = hConst heroX
    newY           = hConst heroY
    (heroX, heroY) = hCurrPos h |+| dirToCoord dir
    hConst i       = max 0 (min i 80)


-- same as before
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

{% endhighlight %}

`handleDir` check's the hero's next position and looks to see if it is
a wall or door. If it is - the hero stays put, otherwise the hero gets to
move. We'll add support for collision with all of our objects when we
build up the combat system.

<br />
### Finalizing Part 2 ### 

Now we should be able to compile `Main.hs` and move our little figure around.
The hero should not be able to move through walls or the closed door. This
has kind of been a long post, but we got some definitions done early for what
the game will become.

Thank you very much for reading, and please leave me any comments you have!

You can find the source to this post [here](https://github.com/jamiltron/Thieflike/tree/post02).