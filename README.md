## Most recent email

Attached are updated scripts for this dataset, along with a few others
I've been working on (still dont know the best way to organize all
these scripts for my datasets).

I think I've figured out my question about computing the voter effect
variables and the party ID/strong partisan variables, but if you could
look them over and give me any comments on form or if you notice
something I should be doing differently that would be dope.

Voter effect variable code start at line 786 in the transform script.
Party ID/strong.partisan scales start at line 346 in the transform
script.  The apprehension scale construction starts at line 81 in
transform script, I still need help figuring out how to deal with
that.

The new questions I'm getting a bit stuck on are largely about
plugging this data into regressions and ggplot.

1) Regressions - the first file is just the linear regression models I
was playing with, but my data violates linear assumptions so I've
moved on to running the regressions in ordinal logit models.  Code for
that is included in the "Regression experiments" script.

My big question here is whether the use of the scale() function on all
of my variables is the best way to standardize the variables I feed
into the regressions. Some IV scales are binary, some have three
levels, some five, and some have seven, so I know that I have to
normalize that for everything to run more smoothly, but I'm unsure
what the right procedure is.  If scale() does work, then is it
sufficient to do it to the final scales, or should I go back and scale
each of the individual items before scaling the variable and plugging
it into the equation?

2) ggplot - I attached a script called "crosstabs" which was
originally just a series of simple bar charts for descriptive
purposes, but near the bottom of that script (line 647) I have been
putting together figures for our APSA paper. Hopefully the outputs are
straightforward enough to understand what I was trying to do, but my
big problem has been incorporating geom_text() to display the actual
percentages for each category (and omitting them when the category is
below a certain %).  Every time I use a template I find on stack
overflow or something it never works for me. I could get it to display
overall percentages, but I need each column to add up to 100%.

I realize now that I probably made more work for myself by subsetting
the data itself by conditions rather than just facet griding them in
the main dataset, but I want to put the plots I've made into one
figure (i.e. all 3 of the perceived bias against Clinton plots in one
figure, and all 3 of the perceived bias against Trump plots in one
figure; or all in one if that is workable - I dont know how to work
with figure margins very well).

The only other question I would have is where to focus my energy
next - I feel pretty comfortable with the set of things I've been
doing with this dataset.  My next task is to really tie down
tidyverse, because mutate() was wildly useful and piping seems super
intuitive and less cumbersome.  Outside of that, what kind of R skills
would be best to cut my teeth on before I have to analyze my next big
dataset? Transforming everything was a total pain in the ass, but
after I got that down, everything else seemed fairly straightforward.

Thanks again for taking the time to do this, I know you've probably
got a billion other things on your plate getting ready for the
semester.
