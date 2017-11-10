---
title: Feedback from Hash Code 2016 Qualification Round
date: 2016-02-12
logo: hashcode
---

![hashcode](images/hashcode2016.jpg)

Yesterday was the qualification round for [Hash Code 2016](https://hashcode.withgoogle.com/) contest, organized by
Google. I already had the opportunity to participate next, and made it to the
final round with my team.  For this new edition, the team and motivation was
the same but we thought a bit about how we could improve our organization to be
more efficient. This post is about what I will take away from this competition.

# The Competition

For this third edition, the concept stays the same, you can participate in
*teams*, and must perform well during the qualification round to make it to the
final round (which takes place in Google headquarters in Paris). Duration of
qualification round is about *4 hours*, and *final is a whole day*. Usually the
difficulty of the problem of qualification is lower than for the final. Though
this year's challenged seemed a bit more complex than the previous one.

# The problems

All the problems from Hash Codes contests are pretty much of the same kind
everytime since they are *optimization problems*. You're given a situation, and
you want to maximize some output of the problem given an initial step:

* **2014 Final**: [Optimize routing of Street view cars](https://hashcode.withgoogle.com/2014/tasks/hashcode2014_final_task.pdf) in a city.
* **2015 Qualification**: The task was about [optimizing a data center](https://hashcode.withgoogle.com/2015/tasks/hashcode2015_qualification_task.pdf).
* **2015 Final**: [Optimize wireless internet coverage](https://hashcode.withgoogle.com/2015/tasks/hashcode2015_final_task.pdf) using Loons.
* **2016 Qualification**: Optimize schedule of drones to deliver orders to customers on a 2D grid.
* **2016 Final**: Optimize something else...

This kind of problems are interesting because it takes a *broad range of skill
and knowledge to solve them*, plus, *you can't find the global optimum* (or at
least not easily/in a reasonnable time).  So the goal of any of this
competitions it to find a better solution than your competitors, or a least, a
good enough solution.

# Team Organization

Organizing a team during such a short period of time, on a complex problem is
challenging. There are a lot of ways you can do it:

* *Everyone is programming*.
* *Split the tasks among team members*.
* *You can define a schedule to brainstorm/code/visualize/give feedback*.
* etc.

Last year, we chose an approach where we would *talk and brainstorm with the
whole team* during the contest. *Not everyone was coding* and one of the team
member did some very *useful visualization* to gain insight, while others were
implementing some ideas from the discussions we had about the challenge.  This
approach was kind of Ok, we made it to the final, but didn't perform very well
during the final round. Here are some short comings:

* Not everyone was at ease with programming.
* Not everyone knows the same programming languages.
* We didn't participate in this kind of competition before.
* Brainstorming a lot can bring a lot of ideas, but *you can't try everything*, so it's better to stick to one or two promising ideas that aren't too difficult to implement in a short time.
* Coding speed matters so you better go with a programming language that you know very well.

We weren't very satisfied with what we did that year, so we tried a different
approach this year. We observed that a lot of teams seem to go with a more
individual approach, where everyone is programming. That's what we tried:

1. Read problem statement, **making sure everyone understood the same things**.
2. Carefully consider the inputs, to **list every information at our disposal**.
3. **Made sure that the score to optimize was clear to everyone**.
4. Initial brainstorm on some solutions, ways to solve the problem.
5. Then everyone tried to implement a working solution, using what we discussed.

We had little interactions after **step 4**, which wasn't a good thing in my
opinion. What we observed is that **we talked less**, and had **a bit less
fun**. We didn't interact as much as last year, and **not everyone felt like
they participated** in the team's work, due to individual solutions.


# Challenges

Here is what I found to be difficult during this qualification round:

* **Your worst ennemy is complexity**.
* *Your second worst is your concentration*.
* Being reasonable about the solution to choose.

What I found useful was to decide early on *a method that isn't too complex* or
too hard to implement. The most important is to *submit a solution*. *Better
have a simple solution that works than a complex one that doesn't*. Your goal
is not to find the best solution ever, but to find a solution *good enough* to
make if to the final round.  Moreover if your first simple solution works, you
should have sufficient time to improve it before the end of the round (if you
don't, that means your solution was maybe too complex, or that you took a big
risk).

One of my teammates, Florian, posted a [blog post](https://flothesof.github.io/thoughts-before-hashcode-2016.html)
this week, before the competition to explain what he thought would be a good
way to tackle the coming qualification. I mainly agree with him, in the light
of what happened yesterday, but I tried to clarify some point in the comments
(that was before the contest). Some of my thoughts on the subject evolved:


> [...] the top-down divide and conquer approach that you describe (as lazy
> evaluation), is more about breaking the problem into a maximum number of small
> sub-parts than it is about modelling the problem. You can see that, for
> example, last year's winners didn't create classes or structs, they just used
> plain C arrays. And this is more about breaking the complexity of the problem,
> and deferring complexity to later as much as you can. This helps keep track of
> what you've done, what you're doing, and what remain to be done, while managing
> complexity: "I know my solution must look like do A, then B, and C at a high
> level, and I know I can do A, B, C later, with the same top-down approach.
> Let's not think about this know, it would waste time and concentration."

What I think now is that, you can go with a low-level language and less
modelization if you are well prepared to program in a short period of time.
Otherwise, you would better stick with the clean step-by-step approach to limit
complexity, and **validate your steps as you go**. It's crucial to limit
potential errors and bugs, so that you are confident your solution will work.
**You can't afford lot of debug in a competition**.

> [...] the code doesn't have to be clean, or readable, in a general way. It
> must be clear to you, at least during the time of the competition, which is
> only a few hours. This is why I think it's not necessary to add comments, or
> create classes/struct/etc. to model your ideas into code. Rather it's a race
> against time and complexity and you can use this kind of programming constructs
> to break the complexity of the problem even further, but not to be readable.
> Breaking into small part is also useful to refactor, try new ideas, change
> stuff, quickly (and I agree that modelling can help too there, but I don't
> think it's a low hanging fruit in this kind of competition) .

This point is similar to the previous one, except that you should not be too
confident in your ability to handle a complex problem/solution in a short
amount of time. **Better be slow and right, than fast and wrong**.

> [...] it's better to have a slow solution that works than a quick solution
> that doesn't. But I would argue that this argument can be held against what you
> say too, because we also can say that it's better to have an ugly/dirty/oneshot
> solution that works, than a beautifully designed/readable solution that
> doesn't. So it's more quick & dirty but works, than high-level/well-designed
> code that doesn't [...]

> [...] it can be an advantage to develop the solution in a fast language like
> C++ or Java, instead of a more dynamic language like Python. I may be wrong,
> but I think you have better chances to have a fast-enough solution in C++ with
> ugly code, no modelling or optimization effort than it is to achieve reasonable
> performance in Python (this is mainly true for the kind of problem we have to
> tackle in the Hash Code: optimization and number crunching). When you have to
> explore big solution spaces looking for an optimum, with little time to think
> about an elegant solution, you're playing against time, so it's good to use a
> language in which you can go quick and dirty, but still get reasonable
> performances.

It's true only if you can come up with a working program in this language,
during the time of the competition. If you don't feel like you can, stick with
the easier, well-known language. **Working but slow solution in *Python* is
better than a fast non-working solution in *C++*.**

# Take-away

**TL;DR**:

* *Master your programming environment*.
* *Manage/avoid complexity if you can*.
* Don't go for a too complex/fancy solution.
* *Communication with your team members is good* if you want to feel like you're competing together.
* **Have fun!**

Now we'll try to improve our solutions during the extended round. I'm eager to
read some blog posts on the problem we had to tackle. Insight from other teams
will definitely be interesting!
