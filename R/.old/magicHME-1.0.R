#NOTE: I ended up getting all of this to work (and the scale worked!) but I get the
#feeling that I'm not writing this code very efficiently or that I'm not thinking through
#my syntax by asking myself the right questions.

#Basically, the scenario is that I'm creaing my conspiracy belief scale from 4 variables
#in the Magical thinking/HME dataset from before.

#any comments?

### file system -
### It's good to keep this stuff organized. I have a folder labelled "r" that I house
###   R related projects. So, the tilde is a linux shortcut for your computer's
###   "home" directory. To see the path to your home directory, run this:
path.expand("~")

### Your current "working directory" is the place in your system of files that R
###   thinks you're "working" from. When you open Rstudio, it will assume you are
###   working from what it defaults to. This is your computer's home directory
###   OR some other location that you can respecify by customizing the setting
###   (click around in Rstudio and you can find something about the "default
###   working directory").

### Based on your code, it appears that you have crated an Rstats folder. If that's
###   your current working directory, then you should be able to read in the data
###   by specifying the location of the data relative to the current working
###   directory.
getwd() ## this should be "/Users/<jumbles>/RStats" or something like that

#Import dataset - I just edited the raw file to remove whatever weird header business
#was going on there, this seemed to load just fine.
##Magic.csv <- read.csv("~/RStats/Magic HME data.csv", header = TRUE)
Magic.csv <- read.csv("Magic HME data.csv", header = TRUE)

#load packages - plyr for the revalue, but idk if there is a more efficient way.
#tidyverse b/c Hadley said so.

### Hadley has a few outdated packages that should be avoided. One is plyr. If
###   you can do something in plyr, then you can do it in "dplyr" (the successor
###   and crown jewell of the tidyverse). Note: hadley actually has a "forcats"
###   package that might be useful in this case. It's all about categorical vars.
### Other outdated packages that come up a lot include "reshape" and "reshape2".
###   The package you want instead of those is "tidyr"

##library(plyr)
library(tidyverse)
library(psych)

### Recoding likert type items is always a bit difficult. It ultimately depends
###   on how you decide to treat them. It seems like most COMS people assume
###   that it's okay to treat 5 or 7-item likert-type questions as numeric. In
###   which case, you'll want to convert them to factor and then to numeric.

### You can shortcut this process below and skip the revalue function by
###   specifying the order of the factor levels directly:
conspiracy1 <- factor(Magic.csv$Q83_1,
                      levels = c("Strongly disagree",
                                 "Somewhat disagree",
                                 "Neither agree nor disagree",
                                 "Somewhat agree",
                                 "Strongly\nagree"))
### And then convert the created factor to numeric:
conspiracy1 <- as.numeric(conspiracy1)

### You can streamline this conversion by creating a user function:
likert2numeric <- function(x) {
    ## convert object x (supplied by user when using this function)
    ## into factor with these likert levels
    x <- factor(x,
                levels = c("Strongly disagree",
                           "Somewhat disagree",
                           "Neither agree nor disagree",
                           "Somewhat agree",
                           "Strongly\nagree"))
    ## now convert x to numeric. since it's the last line of the function,
    ## it's what will be returned.
    as.numeric(x)
}

### Once you've read in the function, you can use it.
conspiracy1 <- likert2numeric(Magic.csv$Q83_1)
conspiracy2 <- likert2numeric(Magic.csv$Q83_2)
conspiracy3 <- likert2numeric(Magic.csv$Q83_3)
conspiracy4 <- likert2numeric(Magic.csv$Q83_4)

### You can simplify this even more by taking four vectors of variables you'd
###   like to convert and applying a function to each of them.
### There are a few diffent "apply"-type of functions. Really, there all different
###   versions of `lapply`, which is code for list apply.
### When dealing with data frames, each column is actually just a special case of
###   a list. So we can specify those four columns:
Q83 <- Magic.csv[, c("Q83_1", "Q83_2", "Q83_3", "Q83_4")]

### And then use lapply to apply the likert2numeric function to each element of
###   the list, Q83.
c_scale <- lapply(Q83, likert2numeric)

### Bingo bango, now convert that to a data frame and you're done
c_scale <- data.frame(c_scale)

### Another apply function is `apply`, which is designed for matrices/data frames.
###   The apply function is different from lapply because it expects you to
###   specify either rows (1) or columns (2).
c_scale <- apply(Q83, 2, likert2numeric)

### The apply function actually returns a matrix, which is a more restrictive
###   version of a data frame. That can cause some trouble, which is why I think
###   it's useful to get a handle of `lapply` first. The lapply function always
###   returns lists, which means you'll have to convert it to the desired class
###   (for example, into a data frame like above) yourself. But I think that's
###   better than being tricked by `apply`. Tho apply is still useful to know!
class(c_scale)
c_scale <- data.frame(c_scale)

### Also, I'm not sure where you got the "c1", "c2" from. perhaps taht was an
###   older named version of "conspiracy1" "conspiracy2" etc. Just make sure
###   as you rename and change code to check how that may affect other parts
###   of your code. It's always a challenge, but it's easier when you're
###   mindful of it throughout a session.

#Scaling and aphla analyses to confirm scales - this solution ended up working for me but,
#again, probably a more efficient way of doing this
##c_scale <- data.frame(as.numeric(c1),
##                      as.numeric(c2),
##                      as.numeric(c3),
##                      as.numeric(c4))

### The method you've used to get reliability works, but you're giving yourself
###   more work than what is necessary. The psych package is the right choice,
###   but your items are already standardized (they are on the same scale),
###   so you can just use psych's `alpha` function directly:
psych::alpha(c_scale)

##scaleKey <- c(1, 1, 1, 1)
##results <- psych::scoreItems(keys = scaleKey, items = c_scale,
##                       totals = FALSE, missing = TRUE, min = 1, max = 5)
##results


### For the record, I didn't memorize what the name of the function was. I
###   actually just searched in R for it, using this search:
help.search("cronbach")

### If you want to browse through a package, try something like this to get
###   a list of all of a package's functions.
library(help = "psych")

### Oh, okay, I see you found out alpha as well. Yeah, the only difference
###   is that alpha assumes the scores are on the same scale. For the record,
###   you can rescale things in base R using the `scale` function.
c_scale[, 1:ncol(c_scale)] <- scale(c_scale)
psych::alpha(c_scale)

#got this to work too, but unsure what the difference is between scoreItems and alpha
##alpha(c_scale)


### Scale can be a bit annoying because it returns a column with attributes
###   "center" and "scaled". So, if you supply one vector, it returns a matrix:
scale(sample(-10:10, 10))

### You can remedy that by specifying scale return the scaled column:
scale(sample(-10:10, 10))[, 1]

