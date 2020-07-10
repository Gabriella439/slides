% How to market Haskell
% Gabriel Gonzalez
% July 31, 2020

<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@4/distr/fira_code.css">
<style>
code {
  font-family: "Fira Code";
}
</style>

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
* The chasm
* Crossing the chasm

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

🧐💭 "I will steer conversations away from Haskell's poor IDE support"

# Misconception #2: Marketing = Hype

Do you believe that popular tools thrive because of __hype__?

🤔: How will I know which database is the best?

😎: All the ❄️cool❄️ kids are using MongoDB.  #JoinTheConvo

People who believe this will conclude:

🧐💭 "I should spend more time cheerleading about Haskell on social media"

# Misconception #3: Marketing = Paid promotion

Do you believe that popular tools thrive because of __corporate backing__?

🤔: Maybe I should just use Postgres instead?

😎: MongoDB just raised $150 million 💰.  If you can't beat 'em, join 'em

People who believe this will conclude:

🧐💭 "Facebook adopting Haskell will save the language"

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

# Marketing is virtuous

Marketing is actually a noble task, which the book captures well in this quote:

> "Marketing's purpose, therefore, is to develop and shape something that is
> real, and not, as people sometimes want to believe, to create illusions.  In
> other words, we are dealing with a discipline more akin to gardening or
> sculpting than, say, to spray painting or hypnotism."

In other words, if you market a tool well, no deception, hype, or promotion is
necessary.

# Questions?

* What is "marketing"?
* The chasm
* Crossing the chasm

# Slow death

Credit: Simon Peyton Jones - "A taste of Haskell"

![](./slow-death.png)

# Immortality

Credit: Simon Peyton Jones - "A taste of Haskell"

![](./immortality.png)

# Stagnation

Credit: Simon Peyton Jones - "A taste of Haskell"

![](./ambiguous.png)

# The Technology Adoption Life Cycle

The technology adoption life cycle models adoption of _discontinuous_ changes

* Continuous change: Upgrading `aeson` to improve runtime performance
* Discontinuous change: Switching from Python to Haskell

__Early adopters__ are visionaries: they prefer new technologies

__Early majority__ are pragmatists: they prefer proven technologies

We won't cover late majority or laggards in this talk

![](./chasm.png)

# The chasm

The "chasm" refers to a large gap between early adopters and the early majority

![](./chasm-2.png)

Symptoms of being in the chasm:

* Burst of enthusiasm and adoption followed by "hitting a wall"
* Early adopters love you, but mainstream adopters are suspicious
* Nothing ever officially supports you

# Why does the chasm exist?

Early adopters are not good references for the early majority

In fact, early adopters are __negative__ references for the early majority

This happens for the following reasons:

* Lack of respect for the value of colleague's experiences

* Taking a greater interest in technology than in their industry

* Failing to recognize the importance of existing product infrastructure

* Overall disruptiveness

# Lack of respect

> Visionaries are the first people in their industry to see the potential of the
> new technology.  Fundamentally, __they see themselves as smarter than their
> opposite numbers in companies__ -- and, quite often, they are.  Indeed, it is
> their ability to see things first that they want to leverage into a
> competitive advantage.  That advantage can only come about if no one else has
> discovered it.  They do not expect, therefore, to be buying a well-tested
> product with an extensive list of industry references.  Indeed, __if such a
> reference base exists, it may actually turn them off__, indicating that for
> this technology, at any rate, they are already too late.

🧐: Haskell is a secret weapon.  Other languages are toys in comparison

> Pragmatists, on the other hand, __deeply value the experience of their
> colleagues__ in other companies.  When they buy, they expect extensive
> references, and they want a good number to come from companies in their own
> industry segment.  This, as we have already noted, creates a catch-22
> situation;  since there are usually only one or two visionaries per industry
> segment, __how can you accumulate the number of references a pragmatist
> requires, when virtually everyone left to call on is also a pragmatist?__

🤔: Then why are so few companies using Haskell?

# Interest in technology

> Visionaries are defining the future.  You meet them at technology conferences 
> and other futurist forums where people gather to forecast trends and seek out
> new market opportunities.  They are easy to strike up a conversation with, and
> they understand and appreciate what high-tech companies and high-tech products
> are trying to do.  They want to talk ideas with bright people.  __They are
> bored with the mundane details__ of their own industries.  __They like to talk
> and think high tech__.

🧐: Profunctor optics are the future of lenses

> Pragmatists, on the other hand, __don't put a lot of stake in futuristic
> things__.  They see themselves more in present-day terms, as the people
> devoted to making the wheels of their industry turn.  Therefore, they tend to
> invest their convention time in industry-specific issues.  Where pragmatists
> are concerned, __sweeping changes and global advantages may make for fine
> speeches, but not much else__.

🤔: I can already write a web server in Python, so why learn a new language?

# Failing to recognize existing infrastructure

> Visionaries are building systems from the ground up.  They are incarnating
> their vision.  __They do not expect to find components for these systems
> lying around.__  They do not expect standards to have been established --
> indeed, they are planning to set new standards.  They do not expect support
> groups to be in place, procedures to have been established, or third parties
> to be available to share in the workload and the responsibility.

🧐: No OAuth package?  No problem; I can probably whip up my own

> Pragmatists expect all these things.  __When they see visionaries going their
> own route with little or no thought of connecting with the mainstream
> practices in their industry, they shudder.__  Pragmatists have based their
> careers on such connections: Once again, it is painfully obvious that
> visionaries, as a group, make a very poor reference base for pragmatists.

🤔: Seriously? No OAuth support?  I'll revisit Haskell it's more mature

# Overall disruptiveness

> From a pragmatist's point of view, visionaries are the people who come in and
> soak up all the budget for their projects.  If the project is a success, they
> take all the credit, while the pragmatists get stuck trying to maintain a
> system that is so "state-of-the-art" no one is quite sure how to keep it
> working.  If the project fails, visionaries always seem to be a step ahead of
> the disaster, getting out of town while they can, and leaving the pragmatists
> to clean up the mess.

🧐: Rewriting this project in Haskell would solve our problems

> Visionaries, successful or not, don't plan to stick around long.  They see
> themselves on a fast track that has them leapfrogging up the corporate ladder
> and across corporations.  Pragmatists, on the other hand, tend to be
> committed long term to their profession and the company at which they work
> They are very cautious about grandiose schemes because they know they will
> have to live with the results.

🤔: Will I have to learn Haskell if you abandon this project?

# Is there any hope?

So how can early adopters like us persuade mainstream adopters?

We don't want to evangelize more (that makes things worse!)

We also don't want to burn out building an enormous ecosystem for free

Instead, we need to understand how _markets_ work (i.e. "marketing")

# Questions?

* What is "marketing"?
* The chasm
* Crossing the chasm

# Crossing the chasm

"Crossing the chasm" refers to jumping the gap from early adopters to the
majority

As noted previously, this appears to be a chicken-and-egg problem:

> Pragmatists, on the other hand, __deeply value the experience of their
> colleagues__ in other companies.  When they buy, they expect extensive
> references, and they want a good number to come from companies in their own
> industry segment.  This, as we have already noted, creates a catch-22
> situation;  since there are usually only one or two visionaries per industry
> segment, __how can you accumulate the number of references a pragmatist
> requires, when virtually everyone left to call on is also a pragmatist?__

... but we can bootstrap positive references from mainstream programmers!

However, doing so requires some marketing basics

# Chain reaction

I like to think of mainstream adoption as analogous to a nuclear chain reaction:

* If the adoption reaches a critical mass then adoption grows uncontrollably

  "the complete absence of death"

* Otherwise adoption "fizzles out"

  "the slow death"

Understanding "market segmentation" helps to stimulate such a chain reaction

# Amplification

When positive references "collide", magic happens:

* 🌱 Organization adopts tool
* 🎓 New developers trained
* 🔨 Contributions made back to ecosystem
* 💖 More positive references created

```
     Before           Middle          After 
┌───────────────┬───────────────┬───────────────┐
│               │               │       ○       │
│               │               │       ↑       │
│    →○  ○←     │    {react}    │    ○←   →○    │
│               │               │       ↓       │
│               │               │       ○       │
└───────────────┴───────────────┴───────────────┘

Legend:

○: Positive reference
●: Negative reference
```

# Cancellation

Negative references neutralize positive references:

🧐: I heard great things about Haskell

🙃: Haskell has been the butt end of a joke for years

```
     Before           Middle          After
┌───────────────┬───────────────┬───────────────┐
│               │               │               │
│               │               │               │
│    →○  ●←     │   {fizzle}    │               │
│               │               │               │
│               │               │               │
└───────────────┴───────────────┴───────────────┘

Legend:

○: Positive reference
●: Negative reference
```

# Segmentation

> A "market" is a group of users that reference each other when making
> decisions.

Positive and negative references mostly stay within their respective markets

```
     Market A        Market B        Market C
┌───────────────┬───────────────┬───────────────┐
│               │            →○ │ ○←            │
│            →● │ ○←            │               │
└───────────────┴───────────────┴───────────────┘

┌───────────────┬───────────────┬───────────────┐
│               │              ○│○              │
│              ●│○              │               │
└───────────────┴───────────────┴───────────────┘
┌───────────────┬───────────────┬───────────────┐
│               │             ○←│→○             │
│             ●←│→○             │               │
└───────────────┴───────────────┴───────────────┘

Legend:

○: Positive reference
●: Negative reference
```

# Slow death

If you spread yourself thin, no chain reaction ever occurs

```
     Market A        Market B        Market C
┌───────────────┬───────────────┬───────────────┐
│ ●        ○    │ ●       ○     │     ○         │
│               │    ○          │○        ● ○   │
│   ○   ●       │               │      ●       ○│ Before
│               │ ○        ●    │   ●           │
│         ○     │               │      ○        │
│    ○          │        ○      │           ●   │
└───────────────┴───────────────┴───────────────┘

┌───────────────┬───────────────┬───────────────┐
│    ●        ○ │  ○     ○      │   ●           │
│      ○        │      ●        │       ○       │
│          ○    │               │   ○           │ After
│ ○    ●        │       ○       │     ●    ●    │
│       ○       │            ●  │○     ○      ○ │
│               │ ○             │     ●         │
└───────────────┴───────────────┴───────────────┘

Legend:

○: Positive reference
●: Negative reference
```

# Slow death

Focus your efforts on one market to trigger a self-sustaining chain reaction

```
     Market A        Market B        Market C
┌───────────────┬───────────────┬───────────────┐
│ ●        ○    │ ●       ○     │     ○         │
│               │    ●          │○        ● ○   │
│   ●   ●       │               │      ○       ○│ Before
│               │ ●        ●    │   ○           │
│         ○     │               │      ○        │
│    ○          │        ○      │           ○   │
└───────────────┴───────────────┴───────────────┘

┌───────────────┬───────────────┬───────────────┐
│        ○      │               │  ○      ○     │
│               │     ●      ●  │ ○  ○       ○  │
│   ○          ●│       ○       │        ○      │ After
│  ●            │               │ ● ○     ○     │
│               │               │  ○    ○ ○   ○ │
│               │        ●      │ ○ ○        ○  │
└───────────────┴───────────────┴───────────────┘

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
