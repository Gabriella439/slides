% How to market Haskell to mainstream programmers
% Gabriella Gonzalez
% August 1, 2020

# Source material

<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/tonsky/FiraCode@4/distr/fira_code.css">
<style>
code {
  font-family: "Fira Code";
}
</style>

This talk draws inspiration from the following book:

* [Crossing the Chasm: Marketing and Selling High-Tech Products to Mainstream Customers](https://www.amazon.com/Crossing-Chasm-Marketing-High-Tech-Mainstream/dp/0060517123)

The book addressed startups, but the advice is highly relevant to Haskell

# Everything you know about marketing is wrong

This is my one 🔥hot take🔥 for the talk

I'm confident that most Haskell programmers don't know how to market to
mainstream programmers

In fact, I'll wager that they will misunderstand marketing _in general_

I'll go through a few marketing misconceptions, using MongoDB as the villain

(Sorry, MongoDB)

# Misconception #1: Marketing = Misleading claims

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

🧐💭 "I should spend more time praising Haskell on social media"

# Misconception #3: Marketing = Corporate backing

Do you believe that popular tools thrive because of __corporate backing__?

🤔: Maybe I should just use Postgres instead?

😎: MongoDB just raised $150 million 💰.  If you can't beat 'em, join 'em

People who believe this will conclude:

🧐💭 "Facebook adopting Haskell will save the language"

# So what is marketing?

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

In other words, if you market a tool well, no misleading claims, hype, nor
corporate backing is necessary.

# The Technology Adoption Life Cycle

The technology adoption life cycle models adoption of _discontinuous_ changes

Early adopters are __visionaries__: they prefer new technologies

Early majority are __pragmatists__: they prefer proven technologies

We won't cover late majority or laggards in this talk

![](./chasm.png)

# The chasm

The "chasm" refers to a large gap between visionaries and pragmatists

![](./chasm-2.png)

Symptoms of being stuck in the chasm:

> * Burst of enthusiasm and adoption followed by "hitting a wall"
> * Visionaries adore you, but pragmatists are suspicious
> * Nothing important ever officially supports you

# Visionaries

Credit: Simon Peyton Jones - "A taste of Haskell"

![](./slow-death.png)

# Pragmatists

Credit: Simon Peyton Jones - "A taste of Haskell"

![](./immortality.png)

# Stuck in the chasm

Credit: Simon Peyton Jones - "A taste of Haskell"

![](./ambiguous.png)

# Why does the chasm exist?

Visionaries are __negative__ references for pragmatists

In other words: evangelism from visionaries turns off pragmatists

Why is that?

# Lack of respect for the value of colleague's experiences

> Visionaries are the first people in their industry to see the potential of the
> new technology.  Fundamentally, they see themselves as smarter than their
> opposite numbers in companies -- and, quite often, they are.  Indeed, it is
> their ability to see things first that they want to leverage into a
> competitive advantage.  That advantage can only come about if no one else has
> discovered it.  __They do not expect__, therefore, __to be buying a
> well-tested product with an extensive list of industry references.  Indeed,
> if such a reference base exists, it may actually turn them off, indicating
> that for this technology, at any rate, they are already too late.__

> __Pragmatists, on the other hand, deeply value the experience of their
> colleagues__ in other companies.  When they buy, they expect extensive
> references, and they want a good number to come from companies in their own
> industry segment.  This, as we have already noted, creates a catch-22
> situation;  since there are usually only one or two visionaries per industry
> segment, how can you accumulate the number of references a pragmatist
> requires, when virtually everyone left to call on is also a pragmatist?

# Taking a greater interest in technology than in their own industry

> Visionaries are defining the future.  You meet them at technology conferences 
> and other futurist forums where people gather to forecast trends and seek out
> new market opportunities.  They are easy to strike up a conversation with, and
> they understand and appreciate what high-tech companies and high-tech products
> are trying to do.  They want to talk ideas with bright people.  __They are
> bored with the mundane details of their own industries.  They like to talk
> and think high tech__.

> __Pragmatists, on the other hand, don't put a lot of stake in futuristic
> things__.  They see themselves more in present-day terms, as the people
> devoted to making the wheels of their industry turn.  Therefore, they tend to
> invest their convention time in industry-specific issues.  Where pragmatists
> are concerned, sweeping changes and global advantages may make for fine
> speeches, but not much else.

# Failing to recognize the importance of existing product infrastructure

> Visionaries are building systems from the ground up.  They are incarnating
> their vision.  __They do not expect to find components for these systems
> lying around.__  They do not expect standards to have been established --
> indeed, they are planning to set new standards.  They do not expect support
> groups to be in place, procedures to have been established, or third parties
> to be available to share in the workload and the responsibility.

> Pragmatists expect all these things.  __When they see visionaries going their
> own route__ with little or no thought of connecting with the mainstream
> practices in their industry, __they shudder.__  Pragmatists have based their
> careers on such connections: Once again, it is painfully obvious that
> visionaries, as a group, make a very poor reference base for pragmatists.

# Overall disruptiveness

> __From a pragmatist's point of view, visionaries are the people who come in and
> soak up all the budget for their projects.  If the project is a success, they
> take all the credit, while the pragmatists get stuck trying to maintain a
> system that is so "state-of-the-art" no one is quite sure how to keep it
> working__.  If the project fails, visionaries always seem to be a step ahead
> of the disaster, getting out of town while they can, and leaving the
> pragmatists to clean up the mess.

> Visionaries, successful or not, don't plan to stick around long.  They see
> themselves on a fast track that has them leapfrogging up the corporate ladder
> and across corporations.  __Pragmatists, on the other hand, tend to be
> committed long term to their profession and the company at which they work.
> They are very cautious about grandiose schemes because they know they will
> have to live with the results.__

# Is all hope lost?

So how on earth can visionaries convince pragmatists to adopt?

We don't want to evangelize more (that makes things worse!)

We also don't want to sell out 💰 or burn out 😰 to satisfy pragmatists

# Visionaries vs. Pragmatists

The solution is to understand the following fundamental difference:

* Visionaries are __project-oriented__

* Pragmatists are __market-oriented__

We need to think/speak/build in a __market-oriented__ way to convince
pragmatists

This is why the discipline is called __market__-ing

# Why do we care about pragmatists?

Pragmatists are picky as hell, but they can become your greatest ally

> …, __once a start-up has earned its spurs with the pragmatist buyers within a
> given market, they tend to be very loyal to it, and even go out of their way
> to help it succeed__.  When this happens, the cost of sales goes way down, and
> the leverage on incremental R&D to support any given customer goes way up.
> That's one of the reasons pragmatists make such a great market.

This is because pragmatists are __market-oriented__

They want their tools to be "official" or "standard" for their industry

# Crossing the chasm

"Crossing the chasm" means jumping the gap between visionaries and pragmatists

![](./chasm-2.png)

As noted previously, this appears to be a chicken-and-egg problem:

> Pragmatists, on the other hand, deeply value the experience of their
> colleagues in other companies.  When they buy, they expect extensive
> references, and they want a good number to come from companies in their own
> industry segment.  This, as we have already noted, creates a catch-22
> situation;  since there are usually only one or two visionaries per industry
> segment, __how can you accumulate the number of references a pragmatist
> requires, when virtually everyone left to call on is also a pragmatist?__

# The recipe for success

* First, solve an enormous problem

  The problem has to be so bad that a few pragmatists _have_ to use your tool

  🤔: I'd rather not try something new, but I have to use Haskell in anger

* Provide a best-in-class experience

  It's not enough to be good, you need to be the best (for that market)

  🤔: Wow, Haskell is way better than I thought.  It blows away the competition

* Focus your effort on one market at a time

  ... because you can't build a top notch experience otherwise

  🤔: Haskell may not be a good fit for other industries, but I'm okay with that

# The perfect pitch

Marketing isn't really about what you say, but here's the "perfect pitch":

> "Haskell is the best tool for X industry.  Everybody's saying great things
> about it.  Ask your colleagues if you don't believe me."

Working backwards from that pitch can help you better understand what to do

# The importance of focus

I like to think of tool adoption as analogous to a nuclear chain reaction:

* If adoption reaches a critical mass within a market then adoption grows
  uncontrollably

  "the complete absence of death"

* Otherwise adoption fizzles out

  "the slow death"

Many understand this but try to manufacture hype to stimulate the reaction

Artificial hype doesn't work because pragmatists are hard to fool

# Amplification

When positive references from pragmatists "collide", magic happens:

* 🌱 Organization adopts tool
* 🎓 New developers trained
* 🔨 Contributions made back to ecosystem
* 📐 Standards drafted
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

# Subcritical markets

If a language community spreads themselves thin, no chain reaction ever occurs

```
      Data
     Science         Finance       Interpreters
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

# Supercritical market

Focus your efforts on one market to trigger a self-sustaining chain reaction

```
      Data
     Science         Finance       Interpreters
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

# Market overlap

Markets are not completely isolated, they slightly overlap

This means that a chain reaction in one market can stimulate an adjacent one

```
                             Interpreters
                          ┌───────────────┐
                 Finance  │  ○○    ○    ○ │
    Data     ┌────────────┼──┐    ○   ○○  │
   Science   │  ○         │ ○│  ○  ○  ○   │
┌────────────┼──┐         │○○│   ●   ○   ○│
│       ●    │○ │      ○  │  │ ○   ○ ○  ○ │
│     ○      │  │  ●      │○ │ ○  ○    ○  │
│            │  │         └──┼────────────┘
│            │  │ ●          │
│  ●         └──┼────────────┘
│        ○      │
└───────────────┘


Legend:

○: Positive reference
●: Negative reference
```

# Market overlap

Markets are not completely isolated, they slightly overlap

This means that a chain reaction in one market can stimulate an adjacent one

```
                             Interpreters
                          ┌───────────────┐
                 Finance  │   ○      ○    │
    Data     ┌────────────┼──┐   ○   ○   ○│
   Science   │  ○    ○ ○  │○○│ ○  ○   ○   │
┌────────────┼──┐  ○      │  │○  ○○       │
│            │○ │ ○○   ○  │ ○│   ○   ●  ○ │
│     ●      │ ○│  ○    ○ │ ○│ ○    ○    ○│
│   ○        │○○│       ○ └──┼────────────┘
│            │○ │ ● ○     ○  │
│     ●      └──┼────────────┘
│           ○   │
└───────────────┘


Legend:

○: Positive reference
●: Negative reference
```

# Market overlap

Markets are not completely isolated, they slightly overlap

This means that a chain reaction in one market can stimulate an adjacent one

```
                             Interpreters
                          ┌───────────────┐
                 Finance  │○  ○       ○●  │
    Data     ┌────────────┼──┐  ○ ○     ○○│
   Science   │   ○  ○     │ ○│○     ○     │
┌────────────┼──┐○ ○○   ○ │  │   ○      ○ │
│ ●    ○   ○○│ ○│   ○ ○   │○ │ ○  ○   ○   │
│     ○○     │○ │         │○○│    ○     ○ │
│ ○      ○   │○ │○○●    ○ └──┼────────────┘
│      ○     │○ │     ○    ○ │
│ ○  ○     ○ └──┼────────────┘
│○       ○   ○  │
└───────────────┘


Legend:

○: Positive reference
●: Negative reference
```

# Which market should we focus on?

Picking a market to focus on is hard

… typically because there are too many good choices!

Should Haskell focus on:

> * __Interpreters?__
> 
>   Haskell's original _raison d'être_ is being the standard language for PLT
> 
> * __Web development?__
> 
>   Michael Snoyman pioneered Haskell in this area, which remains highly active
> 
> * __Data science?__
> 
>   The [Data Haskell](https://www.datahaskell.org/) group was created for this
>   purpose
>
> * __Finance / Cryptocurrency?__
>
>   One of the largest sources of Haskell jobs

# Basics of market selection

The most important factors are:

> * __Target "customer"__
>
>   Is there a single, identifiable person/lead/manager within a software
>   engineering organization with the technical authority to adopt our tool
>   on behalf of the rest of the organization?
>
> * __Compelling reason to adopt__
>
>   Can they live with the problem for another year or not?
>
> * __Whole product__
>
>   Are we close to building  a best-in-class solution to the problem?

Based on that which market do I believe Haskell target?

# Interpreters

If I had to suggest a market, I would focus on interpreters:

> * Target customer
> 
>   One person "team" tasked with building a programmable DSL for a product
> 
> * Compelling reason to adopt
> 
>   JSON/YAML DSLs are collapsing under their own weight (e.g. CloudFormation)
> 
> * Whole product
> 
>   Haskell's ecosystem for interpreters is pretty close to "the total package"

# What about the other options?

> * Web development
>
>   The problem is with the _compelling reason to adopt_.  There are problems
>   with existing web development stacks, but people can live with them.
>
> * Data science
>
>   The problem is with the _whole product_.  We're still far from building an
>   ecosystem that is better than Python.
>
> * Finance / cryptocurrency
>
>   Perhaps this also could have been the selection, but I'm less informed on
>   this market so I can't speak with authority.

# Next steps for interpreters

Here are the remaining things we can do to build the "whole product" for the
interpreter ecosystem:

> * Document and polish interpreter packages
>
>   Focus on: parsing, type-checking, substitution, effect systems, and optics
>
> * Language server that is easy to install and setup
>
>   IDE support is always an important component of the "whole product"
>
> * Provide easy-to-fork GitHub repository implementing an interpreted language
>
>   Preferably one with type inference using best practices
>
> * Publish an authoritative book on how to use Haskell to build an interpreter
>
>   ... and maybe even make some money from doing so!

# Lots of caveats

Please take the upcoming suggestions with a very large grain of salt:

> * I don't mean to suggest that all other work is useless
>
> * Volunteers can do whatever they want with their time
>
>   This advice is for people looking for new ways to contribute
>
> * I could be wrong
>
>   My viewpoint is biased by my own experiences
>
> * Going mainstream is not the be-all and end-all
>
>   We Haskell programmers avoid (success at all costs)

# Conclusion

The important take-away from this talk is to think in a __market-oriented__ way

I can't force people to work on any given market (nor do I want to)

However, I do hope this talk will help people achieve more with less work

Read [Crossing the Chasm](https://www.amazon.com/Crossing-Chasm-Marketing-High-Tech-Mainstream/dp/0060517123) if this talk interested you

* Slides: [https://github.com/Gabriella439/slides/blob/master/marketing/marketing.md](https://github.com/Gabriella439/slides/blob/master/marketing/marketing.md)

![](./marketing_interview.png)
