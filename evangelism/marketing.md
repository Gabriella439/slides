% How to market Haskell
% Gabriel Gonzalez
% July 31, 2020

# Source material

This talk draws inspiration from the following book:

* [Crossing the Chasm: Marketing and Selling High-Tech Products to Mainstream Customers](https://www.amazon.com/Crossing-Chasm-Marketing-High-Tech-Mainstream/dp/0060517123)

The book addressed startups, but the advice is highly relevant to Haskell

I will substantiate the book's message with my own experiences promoting Haskell

# Who needs to hear this?

You are my target audience if you:

* … wish there were more Haskell jobs
* … wonder why Haskell fails to gain mainstream traction

You are not my target audience if you:

* … were hoping for a presentation with code
* … have given up hope on Haskell becoming mainstream

# Overview

* What is "marketing"?
* Segmentation

# Everything you know about marketing is wrong

This is my one 🔥inflammatory🔥 slide for the talk

I'm confident that most of my audience (yes, you!) doesn't know how to market
Haskell to mainstream programmers

In fact, I'll wager that you misunderstand marketing _in general_

I'll go through a few marketing misconceptions, using MongoDB as the "foil"

(Sorry, MongoDB)

# Misconception #1: Marketing = Deception

Do you believe that popular tools thrive because of __misleading claims__?

🤔: Which database I should select for my business?

😎: I recommend MongoDB, the only database that is ✨web scale✨

People who believe this will conclude:

🤓💭 "I will steer conversations away from Haskell's poor IDE support"

# Misconception #2: Marketing = Hype

Do you believe that popular tools thrive because of __hype__?

🤔: How will I know which database is the best?

😎: All the ❄️cool❄️ kids are using MongoDB.  #JoinTheConvo

People who believe this will conclude:

🤓💭 "I should spend more time cheerleading about Haskell on social media"

# Misconception #3: Marketing = Paid promotion

Do you believe that popular tools thrive because of __corporate backing__?

🤔: Maybe I should just use Postgres instead?

😎: MongoDB just raised $150 million 💰.  If you can't beat 'em, join 'em

People who believe this will conclude:

🤓💭 "Facebook adopting Haskell will save the language"

# So what is marketing?

There is a kernel of truth to each misconception, but they each miss the point

The book "Crossing the Chasm" provides an excellent definition of "marketing":

> "Marketing" is prioritizing the needs of a "market".  A "market" is a
> group of users that reference each other when making decisions.

The key concepts are:

* Prioritization: Marketing is about what you choose __not to build__

* Segmentation: Marketing is about who you choose __not to persuade__

In other words, marketing is the art of saying __NO__, either to potential
features or potential users

# Marketing is a force for good

Marketing is actually a noble task, which the book captures well in this quote:

> "Marketing's purpose, therefore, is to develop and shape something that is
> real, and not, as people sometimes want to believe, to create illusions.  In
> other words, we are dealing with a discipline more akin to gardening or
> sculpting than, say, to spray painting or hypnotism."

In other words, if you market a tool well, no deception, hype, or promotion is
necessary.

# Questions?

* What is "marketing"?
* Segmentation

# Traction - Slow death

Credit: Simon Peyton Jones - "A taste of Haskell"

![](./slow-death.png)

# Traction - Immortality

Credit: Simon Peyton Jones - "A taste of Haskell"

![](./immortality.png)

# Why are outcomes so bimodal?

I like to think of tool adoption as analogous to a nuclear chain reaction:

* If the adoption reaches a critical mass then adoption grows uncontrollably

  What Simon Peyton Jones calls "the complete absence of death"

* Otherwise adoption "fizzles out"

  What Simon Peyton Jones calls "the slow death"

Market "segmentation" is a general trick to stimulate a chain reaction

# Amplification

When positive references "collide", magic happens:

* 🌱 Organization adopts tool
* 🎓 New developers trained
* 🔨 Contributions made back to ecosystem
* 💖 More positive references created

```
     Before:         Middle:         After:
+---------------+---------------+---------------+
|               |               |               |
|               |               |      ○        |
|               |               |      |        |
|    -○  ○-     |      💥       |   ○-   -○     |
|               |               |      |        |
|               |               |      ○        |
|               |               |               |
+---------------+---------------+---------------+

Legend:

○: Positive reference
●: Negative reference
```

# Cancellation

Negative references neutralize positive references:

🤓: I heard great things about Haskell

🙃: I thought nobody used Pascal these days

```
     Before:         Middle:         After:
+---------------+---------------+---------------+
|               |               |               |
|               |               |               |
|               |               |               |
|    -○  ●-     |      🌧       |               |
|               |               |               |
|               |               |               |
|               |               |               |
+---------------+---------------+---------------+

Legend:

○: Positive reference
●: Negative reference
```

# Segmentation

Positive and negative references stay within their respective markets

```
     Market A        Market B        Market C
+---------------+---------------+---------------+
|               |            -○ | ○-            |
|               |               |               |  Before
|            -● | ○-            |               |
+---------------+---------------+---------------+

+---------------+---------------+---------------+
|               |              ○|○              |
|               |               |               |  Middle
|              ●|○              |               |
+---------------+---------------+---------------+

+---------------+---------------+---------------+
|               |            ○- | -○            |
|               |               |               |  After
|            ●- | -○            |               |
+---------------+---------------+---------------+

Legend:

○: Positive reference
●: Negative reference
```

# Spreading yourself thin

Positive and negative references tend to stay 

```
    Market A        Market B        Market C
+---------------+---------------+---------------+
| ○             |               |       ●-    ○ |
|               |   ●-          |               |
|      -●  ○-   |        |      |  ○            |
|               |        ○      |         -●    |
|      ○        |               |       |       |
|               |               |       ●       |
|               |   -○          |               |
|    ●          |               |               |
|        -○     |               |               |
|  ○            |               |      -○   ○-  |
|  |            |               |   -●          |
+---------------+---------------+---------------+

Legend:

○: Positive reference
●: Negative reference
```

# Scratch

```
    Market A        Market B        Market C
+---------------+---------------+---------------+
|               |               |               |
|               |               |               |
|               |               |               |
|               |               |               |
|               |               |               |
|               |               |               |
|               |               |               |
|               |               |               |
|               |               |               |
|               |               |               |
|               |               |               |
+---------------+---------------+---------------+

Legend:

○: Positive reference
●: Negative reference
```

# Ideas

* Why did Rust succeed much more quickly than Haskell?
* Crib ideas from podcast discussion
* Critique "Simple Haskell" movement

# TODO

* Use something other than `pandoc` to render the slides
