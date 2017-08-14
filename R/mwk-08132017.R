## mike's script for magicHME project
## 08/13/2017
Rscripts <- list.files()
Rscripts

con <- file(grep("transform", Rscripts, value = TRUE))
x <- readLines(con)
close(con)

##----------------------------------------------------------------------------##
## VOTER EFFECT
##----------------------------------------------------------------------------##
## jf: line 786, transform script

##
## FOLEY CODE
#Effect on voter likelihood scales - Trump 
# 1 = less likely to vote, 3 = more likely to vote 
cat2num.trump.effect <- function(y) {
  lvs <- c(
    "Articles like this one make Trump supporters less likely to vote", 
    "Articles like this one do not affect Trump supporters&#39; decision to vote",
    "Articles like this one make Trump supporters more likely to vote"
  )
  y <- factor(y, levels = lvs) 
  as.numeric(y)
}

trump.effect.iowa <- cat2num.trump.effect(x$effect_trump_iowa) 
trump.effect.nc <- cat2num.trump.effect(x$effect_trump_NC) 
trump.effect.ohio <- cat2num.trump.effect(x$effect_trump_ohio)  
#Effect on voter likelihood scales - Clinton 
# 1 = less likely to vote, 3 = more likely to vote 
cat2num.clinton.effect <- function(y) {   y <- factor(y, 
              levels = c("Articles like this one make Clinton supporters less likely to vote", 
                         "Articles like this one do not affect Clinton supporters&#39; decision to vote", 
                         "Articles like this one make Clinton supporters more likely to vote")) 
   as.numeric(y) }   
clinton.effect.iowa <- cat2num.trump.effect(x$effect_clinton_iowa) 
clinton.effect.nc <- cat2num.trump.effect(x$effect_clinton_NC) 
clinton.effect.ohio <- cat2num.trump.effect(x$effect_clinton_ohio)  
#Effect on Clinton Voters 
x <- mutate(x, vote.effect.clinton = ifelse((x$effect_clinton_ohio == "Articles like this one make Clinton supporters less likely to vote") | 
                                            (x$effect_clinton_iowa == "Articles like this one make Clinton supporters less likely to vote") | 
                                            (x$effect_clinton_NC == "Articles like this one make Clinton supporters less likely to vote"), 
                                             "Less likely to vote", 
                                     ifelse((x$effect_clinton_ohio == "Articles like this one do not affect Clinton supporters&#39; decision to vote") |  
                                            (x$effect_clinton_iowa == "Articles like this one do not affect Clinton supporters&#39; decision to vote") | 
                                            (x$effect_clinton_NC == "Articles like this one do not affect Clinton supporters&#39; decision to vote"), 
                                             "No effect on supporters", 
                                     ifelse((x$effect_clinton_ohio == "Articles like this one make Clinton supporters more likely to vote") |  
                                            (x$effect_clinton_iowa == "Articles like this one make Clinton supporters more likely to vote") | 
                                            (x$effect_clinton_NC == "Articles like this one make Clinton supporters more likely to vote"), 
                                            "More likely to vote", 
                                            NA))))  
vote.effect.clinton <- factor(x$vote.effect.clinton, 
                              levels = c("Less likely to vote", 
                                         "No effect on supporters", 
                                         "More likely to vote"))  
num.vote.effect.clinton <- as.numeric(vote.effect.clinton)  
#Effect on Trump voters 
x <- mutate(x, vote.effect.trump = ifelse((x$effect_trump_ohio == "Articles like this one make Trump supporters less likely to vote") | 
                                         (x$effect_trump_iowa == "Articles like this one make Trump supporters less likely to vote") | 
                                         (x$effect_trump_NC == "Articles like this one make Trump supporters less likely to vote"), 
                                         "Less likely to vote", 
                                   ifelse((x$effect_trump_ohio == "Articles like this one do not affect Trump supporters&#39; decision to vote") |  
                                         (x$effect_trump_iowa == "Articles like this one do not affect Trump supporters&#39; decision to vote") | 
                                         (x$effect_trump_NC == "Articles like this one do not affect Trump supporters&#39; decision to vote"), 
                                         "No effect on voters", 
                                   ifelse((x$effect_trump_ohio == "Articles like this one make Trump supporters more likely to vote") |  
                                         (x$effect_trump_iowa == "Articles like this one make Trump supporters more likely to vote") | 
                                         (x$effect_trump_NC == "Articles like this one make Trump supporters more likely to vote"), 
                                         "More likely to vote", 
                                         NA))))  
vote.effect.trump <- factor(x$vote.effect.trump, 
                              levels = c("Less likely to vote", 
                                         "No effect on voters", 
                                         "More likely to vote")) 
num.vote.effect.trump <- as.numeric(vote.effect.trump)







##----------------------------------------------------------------------------##
## PARTY ID/STRONG.PARTISAN
##----------------------------------------------------------------------------##
## jf: line 346, transform script


##----------------------------------------------------------------------------##
## APPREHENSION SCALE
##----------------------------------------------------------------------------##
## jf: apprehension scale, line 81, transform script
