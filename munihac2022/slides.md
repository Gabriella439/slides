% Minmaxing Slay the Spire with Haskell
% Gabriella Gonzalez
% October 7, 2016

# Background

[My last MuniHac presentation](https://youtu.be/6a5Ti0r8Q2s) was more
theoretical

This time I'm giving a more Hackathon-oriented talk 

Specifically, I'm presenting the project I'm hacking on

## The project

This is a toy attempt to solve the game "Slay the Spire"

I'm only attempting to solve simple early-game fights

No heuristics; we're computing the optimal outcome

## I like to overthink this game

![](./images/Probability post.png)

# Outline

* <span class="fragment highlight-red">Slay the Spire 101</span>
* Probability monad
* Memoization
* Implementing game mechanics

# Mechanics

We're solving a tiny subset of the game:

- Only one character: The Ironclad
- Only one fight: a Cultist

## Turn 1 - Begin

![](./images/Turn 1 - Begin.png)

## Turn 1 - Deck

![](./images/Turn 1 - Deck.png)

## Turn 1 - Strike

![](./images/Turn 1 - Strike.png)

## Turn 1 - Defend

![](./images/Turn 1 - Defend.png)

## Turn 1 - End

![](./images/Turn 1 - End.png)

## Turn 2 - Begin

![](./images/Turn 2 - Begin.png)

## Turn 2 - Deck

![](./images/Turn 2 - Deck.png)

## Turn 2 - Bash

![](./images/Turn 2 - Bash.png)

## Turn 2 - Bash consequences

![](./images/Turn 2 - Bash consequences.png)

## Turn 2 - Defend

![](./images/Turn 2 - Defend.png)

## Turn 2 - End

![](./images/Turn 2 - End.png)

## Turn 3 - Begin

![](./images/Turn 3 - Begin.png)

## Turn 3 - Deck

![](./images/Turn 3 - Deck.png)

## Turn 3 - Strike

![](./images/Turn 3 - Strike.png)

## Turn 3 - End

![](./images/Turn 3 - End.png)

## Turn 4 - Begin

![](./images/Turn 4 - Begin.png)

# Outcome

We only took 2 damage 🎉

* Was that the optimal play? (yes)

* Was that outcome above or below average? (above)

* How much health would I save if I were to upgrade Bash?

# Outline

* Slay the Spire 101
* <span class="fragment highlight-red">Probability monad</span>
* Memoization
* Implementing game mechanics

# Probability monad

We need a way to model uncertain outcomes

We'll do so using the following types:

```haskell
-- | A single possibility, consisting of an outcome paired
--   with the associated weight of that outcome
data Possibility a =
    Possibility { outcome :: a, weight :: !Int }

-- | A probability distribution, which is a non-empty list of
--   weighted outcomes
newtype Distribution a =
    Distribution { possibilities :: NonEmpty (Possibility a) }
```
