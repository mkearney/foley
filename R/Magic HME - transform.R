#clear environment
rm(list = ls())

#load packages
library(psych)

#load dataset
x <- read.csv("~/RStats/Magic HME data - clean.csv", header = TRUE)

#Data by condition
x.ohio <- subset(x, Condition == "Ohio")
x.iowa <- subset(x, Condition == "Iowa")
x.nc <- subset(x, Condition == "North Carolina")

#Race Scale construction - Need to revise to be less fucked up - just needed a quick and dirty way to 
#use in preliminary regressions
x$race_ethnicity
white <- as.numeric(x$race_ethnicity == "White/Caucasian")
white

#Conspiracy scale construction
# max = strongly agree
l2num.agree5 <- function(y) { #create function to convert 5-point agree/disagree scale
  y <- factor(y,
              levels = c("Strongly disagree",
                         "Somewhat disagree",
                         "Neither agree nor disagree",
                         "Somewhat agree",
                         "Strongly agree"))
  as.numeric(y)
}

l2num.agree5.adjust <- function(y) { #create function to convert 5-point agree/disagree scale
  y <- factor(y,
              levels = c("Strongly disagree",
                         "Somewhat disagree",
                         "Neither agree nor disagree",
                         "Somewhat agree",
                         "Strongly\nagree"))
  as.numeric(y)
}

consp1 <- l2num.agree5.adjust(x$consp1)
consp2 <- l2num.agree5.adjust(x$consp2)
consp3 <- l2num.agree5.adjust(x$consp3)
consp4 <- l2num.agree5.adjust(x$consp4)

consp.scale <- data.frame(consp1, 
                          consp2, 
                          consp3, 
                          consp4)

alpha(consp.scale)

consp.scale <- ((consp1 + consp2 + consp3 + consp4)/4)

#Paranormal beliefs scale construction
# max = yes
l2num.yesno3 <- function(y) {
  y <- factor(y,
              levels = c("No",
                         "Not sure",
                         "Yes"))
  as.numeric(y)
}

para1 <- l2num.yesno3(x$para1)
para2 <- l2num.yesno3(x$para2)
para3 <- l2num.yesno3(x$para3)
para4 <- l2num.yesno3(x$para4)

para.scale <- data.frame(para1, 
                         para2, 
                         para3, 
                         para4)

alpha(para.scale)

para.scale <- ((para1 + para2 + para3 + para4)/4)

#Apprehension Scale construction
l2num.freq5 <- function(y) {
  y <- factor(y,
              levels = c("Never",
                         "Seldom",
                         "Sometimes",
                         "Usually",
                         "Always"))
  as.numeric(y)
}

#These first two apprehension Q's were presented correctly in the survey.
#applied the bigger function here, that includes both responses in 1-2 and 3-8.

l2num.freq5.fail <- function(y) {
  y <- factor(y,
              levels = c("Never",
                         "Seldom",
                         "Sometimes",
                         "Usually",
                         "Most of the time",
                         "Always"))
  as.numeric(y)
}

appr1 <- l2num.freq5.fail(x$appr1_seatbelt)
appr2 <- l2num.freq5.fail(x$appr2_wash)
appr3 <- l2num.freq5.fail(x$appr3_luggage)
appr4 <- l2num.freq5.fail(x$appr4_shred)
appr5 <- l2num.freq5.fail(x$appr5_lockcar)
appr6 <- l2num.freq5.fail(x$appr6_lockhouse)
appr7 <- l2num.freq5.fail(x$appr7_cancer)
appr8 <- l2num.freq5.fail(x$appr8_die)

appr.scale <- data.frame(appr1, appr2)
alpha(appr.scale)
summary(appr.scale)
appr.scale.fail <- data.frame(appr1, appr2, appr3, appr4, appr5, appr6, appr7, appr8)
alpha(appr.scale.fail)
summary(appr.scale.fail)

appr.scale.fail <- ((appr1 + appr2 + appr3 + appr4 + appr5 + appr6 + appr7 + appr8)/8)


# Election news attention scale construction
# max = a great deal
l2num.amount5 <- function(y) {
  y <- factor(y,
              levels = c("None at all",
                         "A little",
                         "A moderate amount",
                         "A lot",
                         "A great deal"))
  as.numeric(y)
}

election.news.attn <- l2num.amount5(x$election_news_attention)

#Election interest scale construction
# max = very interested
l2num.interest4 <- function(y) {
  y <- factor(y,
              levels = c("Not at all interested",
                         "Somewhat interested",
                         "Moderately interested",
                         "Very interested"))
  as.numeric(y)
}

election.interest <- l2num.interest4(x$election_interest)

#Religious services scale construction
# max = attends more often
l2num.freq6 <- function(y){
  y <- factor(y,
              levels = c("Never",
                         "A few times a year",
                         "Once or twice a month",
                         "Almost every week",
                         "Every week",
                         "More than once a week"))
  as.numeric(y)
}

rel.services <- l2num.freq6(x$religion_services)

#Big Five scale construction
# max = strongly agree

#Normal Items
bf.agree2 <- l2num.agree5.adjust(x$BF_agree2)
bf.agree4 <- l2num.agree5.adjust(x$BF_agree4)
bf.agree5 <- l2num.agree5.adjust(x$BF_agree5)
bf.agree7 <- l2num.agree5.adjust(x$BF_agree7)
bf.agree9 <- l2num.agree5(x$BF_agree9)
bf.consc1 <- l2num.agree5.adjust(x$BF_consc1)
bf.consc3 <- l2num.agree5.adjust(x$BF_consc3)
bf.consc6 <- l2num.agree5.adjust(x$BF_consc6)
bf.consc7 <- l2num.agree5(x$BF_consc7)
bf.consc8 <- l2num.agree5(x$BF_consc8)
bf.extro1 <- l2num.agree5.adjust(x$BF_extro1)
bf.extro3 <- l2num.agree5.adjust(x$BF_extro3)
bf.extro4 <- l2num.agree5.adjust(x$BF_extro4)
bf.extro6 <- l2num.agree5.adjust(x$BF_extro6)

bf.extro8 <- l2num.agree5(x$BF_extro8)
bf.neuro1 <- l2num.agree5.adjust(x$BF_neuro1)
bf.neuro3 <- l2num.agree5.adjust(x$BF_neuro3)
bf.neuro4 <- l2num.agree5.adjust(x$BF_neuro4)
bf.neuro6 <- l2num.agree5.adjust(x$BF_neuro6)
bf.neuro8 <- l2num.agree5(x$BF_neuro8)
bf.open1 <- l2num.agree5.adjust(x$BF_open1)
bf.open2 <- l2num.agree5.adjust(x$BF_open2)
bf.open3 <- l2num.agree5.adjust(x$BF_open3)
bf.open4 <- l2num.agree5.adjust(x$BF_open4)
bf.open5 <- l2num.agree5.adjust(x$BF_open5)
bf.open6 <- l2num.agree5.adjust(x$BF_open6R)
bf.open8 <- l2num.agree5(x$BF_open8)
bf.open10 <- l2num.agree5(x$BF_open10)

#Reverse coding BF items
l2num.agree5.adjust.reverse <- function(y) { #create function to convert 5-point agree/disagree scale
  y <- factor(y,
              levels = c("Strongly\nagree",
                         "Somewhat agree",
                         "Neither agree nor disagree",
                         "Somewhat disagree",
                         "Strongly disagree"))
  as.numeric(y)
}

l2num.agree5.reverse <- function(y) { #create function to convert 5-point agree/disagree scale
  y <- factor(y,
              levels = c("Strongly agree",
                         "Somewhat agree",
                         "Neither agree nor disagree",
                         "Somewhat disagree",
                         "Strongly disagree"))
  as.numeric(y)
}

bf.agree1 <- l2num.agree5.adjust.reverse(x$BF_agree1R)
bf.agree3 <- l2num.agree5.adjust.reverse(x$BF_agree3R)
bf.agree6 <- l2num.agree5.adjust.reverse(x$BF_agree6R)
bf.agree8 <- l2num.agree5.reverse(x$BF_agree8R)
bf.consc2 <- l2num.agree5.adjust.reverse(x$BF_consc2R)
bf.consc4 <- l2num.agree5.adjust.reverse(x$BF_consc4R)
bf.consc5 <- l2num.agree5.adjust.reverse(x$BF_consc5R)
bf.consc9 <- l2num.agree5.reverse(x$BF_consc9R)
bf.extro2 <- l2num.agree5.adjust.reverse(x$BF_extro2R)
bf.extro5 <- l2num.agree5.adjust.reverse(x$BF_extro5R)
bf.extro7 <- l2num.agree5.adjust.reverse(x$BF_extro7R)
bf.neuro2 <- l2num.agree5.adjust.reverse(x$BF_neuro2R)
bf.neuro5 <- l2num.agree5.adjust.reverse(x$BF_neuro5R)
bf.neuro7 <- l2num.agree5.reverse(x$BF_neuro7R)
bf.open7 <- l2num.agree5.reverse(x$BF_open7R)
bf.open9 <- l2num.agree5.reverse(x$BF_open9R)

#Test scale reliability
bf.scale.all <- data.frame(bf.agree1, bf.agree2, bf.agree3,
                           bf.agree4, bf.agree5, bf.agree6,
                           bf.agree7, bf.agree8, bf.consc1,
                           bf.consc2, bf.consc3, bf.consc4,
                           bf.consc5, bf.consc6, bf.consc7,
                           bf.consc8, bf.consc9, bf.extro1,
                           bf.extro2, bf.extro3, bf.extro4,
                           bf.extro5, bf.extro6, bf.extro7,
                           bf.extro8, bf.neuro1, bf.neuro2,
                           bf.neuro3, bf.neuro4, bf.neuro5,
                           bf.neuro6, bf.neuro7, bf.neuro8,
                           bf.open1, bf.open2, bf.open3,
                           bf.open4, bf.open5, bf.open6,
                           bf.open7, bf.open8, bf.open9,
                           bf.open10)

bf.scale.agree <- data.frame(bf.agree1, bf.agree2, bf.agree3,
                           bf.agree4, bf.agree5, bf.agree6,
                           bf.agree7, bf.agree8)

bf.scale.consc <- data.frame(bf.consc1,
                           bf.consc2, bf.consc3, bf.consc4,
                           bf.consc5, bf.consc6, bf.consc7,
                           bf.consc8, bf.consc9)

bf.scale.extro <- data.frame(bf.extro1,
                             bf.extro2, bf.extro3, bf.extro4,
                             bf.extro5, bf.extro6, bf.extro7,
                             bf.extro8)

bf.scale.neuro <- data.frame(bf.neuro1, bf.neuro2,
                             bf.neuro3, bf.neuro4, bf.neuro5,
                             bf.neuro6, bf.neuro7, bf.neuro8)

bf.scale.open <- data.frame(bf.open1, bf.open2, bf.open3,
                            bf.open4, bf.open5, bf.open6,
                            bf.open7, bf.open8, bf.open9,
                            bf.open10)

alpha(bf.scale.agree)
alpha(bf.scale.consc)
alpha(bf.scale.extro)
alpha(bf.scale.neuro)
alpha(bf.scale.open)

#Drop poor performing questions from scale:
#  bf.extro7, open6, open7, open 8

bf.scale.agree <- data.frame(bf.agree1, bf.agree2, bf.agree3,
                             bf.agree4, bf.agree5, bf.agree6,
                             bf.agree7, bf.agree8)

bf.scale.consc <- data.frame(bf.consc1,
                             bf.consc2, bf.consc3, bf.consc4,
                             bf.consc5, bf.consc6, bf.consc7,
                             bf.consc8, bf.consc9)

bf.scale.extro <- data.frame(bf.extro1,bf.extro2, bf.extro3, 
                             bf.extro4,bf.extro5, bf.extro6, 
                             bf.extro8)

bf.scale.neuro <- data.frame(bf.neuro1, bf.neuro2,
                             bf.neuro3, bf.neuro4, bf.neuro5,
                             bf.neuro6, bf.neuro7, bf.neuro8)

bf.scale.open <- data.frame(bf.open1, bf.open2, bf.open3,
                            bf.open4, bf.open5, bf.open8,
                            bf.open10)

alpha(bf.scale.agree)
alpha(bf.scale.consc)
alpha(bf.scale.extro)
alpha(bf.scale.neuro)
alpha(bf.scale.open)

#Collapse scales for regression
bf.scale.agree <- ((bf.agree1 + bf.agree2 + bf.agree3 +
                    bf.agree4 + bf.agree5 + bf.agree6 +
                    bf.agree7 + bf.agree8)/8)

describe(bf.scale.agree)

bf.scale.consc <- ((bf.consc1 + bf.consc2 + bf.consc3 + 
                    bf.consc4 + bf.consc5 + bf.consc6 + 
                    bf.consc7 + bf.consc8 + bf.consc9)/9)

describe(bf.scale.consc)

bf.scale.extro <- ((bf.extro1 + bf.extro2 + bf.extro3 + 
                    bf.extro4 + bf.extro5 + bf.extro6 + 
                    bf.extro8)/7)

describe(bf.scale.extro)

bf.scale.neuro <- ((bf.neuro1 + bf.neuro2 + bf.neuro3 + 
                    bf.neuro4 + bf.neuro5 + bf.neuro6 + 
                    bf.neuro7 + bf.neuro8)/8)

describe(bf.scale.neuro)

bf.scale.open <- ((bf.open1 + bf.open2 + bf.open3 + 
                   bf.open4 + bf.open5 + bf.open8 +
                   bf.open10)/7)

describe(bf.scale.open)

#partyID 
max = Republican
partyID <- factor(x$partyID,
                  levels = c("Democrat",
                             "Independent",
                             "Republican"))
partyID <- as.numeric(partyID)

x <- mutate(x, partyID.scale = ifelse((partyID == "Democrat") & 
                                        (partyID_strength == "Strong ${q://QID83/ChoiceGroup/SelectedChoices}"), "Strong Democrat",
                               ifelse((partyID == "Democrat") &
                                        (partyID_strength == "Not a strong  ${q://QID83/ChoiceGroup/SelectedChoices}"), "Democrat",
                               ifelse((partyID == "Independent") &
                                        (partyID_lean == "Closer to the Democratic party"), "Weak Democrat",
                               ifelse((partyID == "Independent") &
                                        (partyID_lean == "Closer to the Republican party"), "Weak Republican",
                               ifelse((partyID == "Republican") &
                                        (partyID_strength == "Not a strong  ${q://QID83/ChoiceGroup/SelectedChoices}"), "Republican",
                               ifelse((partyID == "Republican") & 
                                        (partyID_strength == "Strong ${q://QID83/ChoiceGroup/SelectedChoices}"), "Strong Democrat", 
                               NA)))))))

partyID.scale <- factor(x$partyID.scale,
                        levels = c("Strong Democrat",
                                   "Democrat",
                                   "Weak Democrat",
                                   "Weak Republican",
                                   "Republican",
                                   "Strong Republican"))

partyID.scale <- as.numeric(partyID.scale)
partyID.scale

x <- mutate(x, strong.partisan = ifelse((partyID.scale == "Strong Democrat") | (partyID.scale == "Strong Republican"), "Strong Partisan",
                              ifelse((partyID.scale == "Democrat") | (partyID.scale == "Republican"), "Partisan",
                              ifelse((partyID.scale == "Weak Democrat") | (partyID.scale == "Weak Republican"), "Weak Partisan",
                              NA))))

strong.partisan <- factor(x$strong.partisan,
                          levels = c("Strong Partisan",
                                     "Partisan",
                                     "Weak Partisan"))
strong.partisan <- as.numeric(strong.partisan)
strong.partisan
dim(x$strong.partisan)


#Sex variable conversion
# Male = 1, Female = 2
cat2num.sex <- function(y) {
  y <- factor(y,
              levels = c("Male",
                         "Female")) 
  as.numeric(y)
}

sex <- cat2num.sex(x$sex)

#
l2num.income <- function(y) {
  y <- factor(y,
              levels = c("Male",
                         "Female")) 
  as.numeric(y)
}

cat2num.sex(x$sex)

#Income variable scale construction
# 1 = Less than $15,000, 5 = 150,001+
l2num.income8 <- function(y) {
  y <- factor(y,
              levels = c("Less than $15,000",
                         "$15,001-$30,000",
                         "$30,001-$45,000",
                         "$45,001-$60,000",
                         "$60,001-$75,000",
                         "$75,001-$100,000",
                         "$100,001-$150,000",
                         "$150,001 or more")) 
  as.numeric(y)
}

income <- l2num.income8(x$income)
describe(income)

#Education variable scale construction
#1 = less than HS, 6 = Postgraduate degree
l2num.edu6 <- function(y) {
  y <- factor(y,
              levels = c("Less than high school",
                         "High school graduate",
                         "Some college",
                         "2 year degree",
                         "4 year degree",
                         "Postgraduate degree"))
  as.numeric(y)
}

edu <- l2num.edu6(x$education)
edu <- scale(edu)
print(edu)
#Economic partisanship scale construction
# 1 = very liberal, 7 = very conservative
l2num.partisanship7 <- function(y) {
  y <- factor(y,
              levels = c("Very liberal",
                         "Liberal",
                         "Somewhat liberal",
                         "Moderate",
                         "Somewhat conservative",
                         "Conservative",
                         "Very conservative"))
  as.numeric(y)
}

partisanship.econ <- l2num.partisanship7(x$partisanship_econ)

describe(partisanship.econ)

#Social partisanship scale construction
# 1 = very liberal, 7 = very conservative
partisanship.social <- l2num.partisanship7(x$partisanship_social)
describe(partisanship.social)

partisanship.scale <- data.frame(partisanship.econ, partisanship.social)
alpha(partisanship.scale)
partisanship.scale <- ((partisanship.econ + partisanship.social)/2)

#Pessimism scale construction
#max = more likely to experience a pessimistic event
l2num.likely5 <- function(y) {
  y <- factor(y,
              levels = c("Extremely unlikely",
                         "Somewhat unlikely",
                         "Neither likely nor unlikely",
                         "Somewhat likely",
                         "Extremely\nlikely"))
  as.numeric(y)
}

pess.recession <- l2num.likely5(x$pess1_recession)
pess.ebola <- l2num.likely5(x$pess2_ebola)
pess.terror <- l2num.likely5(x$pess3_terror)
pess.war <- l2num.likely5(x$pess4_war)

pess.scale <- data.frame(pess.recession, 
                           pess.ebola, 
                           pess.terror, 
                           pess.war)

alpha(pess.scale)

pess.scale <- ((pess.recession + pess.ebola + pess.terror + pess.war)/4)

#Symbolic Thinking scale construction
# 1 = symbolic, 2 = tangible
symb1 <- factor(x$symb1,
                levels = c("stab a photo of your family six times",
                           "stick your hands in a bowl of cockroaches"))

symb1 <- as.numeric(symb1)
symb1

symb2 <- factor(x$symb2,
                levels = c("a luxurious house where a family had recently been murdered",
                           "a grimy bus station"))

symb2 <- as.numeric(symb2)
symb2

symb3 <- factor(x$symb3,
                levels = c("leave trash on someone's grave",
                           "stand in line for 3 hours at the DMV?"))

symb3 <- as.numeric(symb3)
symb3

symb4 <- factor(x$symb4,
                levels = c("yell \"I hope I die tomorrow\" out loud six times",
                           "ride in a speeding car without a seat belt"))

symb4 <- as.numeric(symb4)
symb4

symb5 <- factor(x$symb5,
                levels = c("sleep in laundered pajamas once worn by Charles Manson",
                           "put a nickel in your mouth that you found on the ground"))

symb5 <- as.numeric(symb5)
symb5

symb6 <- factor(x$symb6,
                levels = c("sold two winning tickets in the past three years but had a long line",
                           "never sold a winning ticket but had no lines"))

symb6 <- as.numeric(symb6)
symb6

symb.scale <- data.frame(symb1,
                         symb2,
                         symb3,
                         symb5)

alpha(symb.scale)

symb.scale <- ((symb1 + symb2 + symb3 + symb5)/4)

#Need for Cognition scale construction
l2num.describes.me5 <- function(y) {
  y <- factor(y,
              levels = c("Does not describe me",
                         "Describes me slightly well",
                         "Describes me moderately well",
                         "Describes me very well",
                         "Describes me extremely well"))
  as.numeric(y)
}

l2num.describes.me5.reverse <- function(y) {
  y <- factor(y,
              levels = c("Describes me extremely well",
                         "Describes me very well",
                         "Describes me moderately well",
                         "Describes me slightly well",
                         "Does not describe me"))
  as.numeric(y)
}

nfc1 <- l2num.describes.me5(x$NFC1)
nfc2 <- l2num.describes.me5(x$NFC2)
nfc3 <- l2num.describes.me5.reverse(x$NFC3R)
nfc4 <- l2num.describes.me5.reverse(x$NFC4R)
nfc5 <- l2num.describes.me5.reverse(x$NFC5R)
nfc6 <- l2num.describes.me5(x$NFC6)
nfc7 <- l2num.describes.me5.reverse(x$NFC7R)
nfc8 <- l2num.describes.me5.reverse(x$NFC8R)
nfc9 <- l2num.describes.me5(x$NFC9)
nfc10 <- l2num.describes.me5.reverse(x$NFC10R)
nfc11 <- l2num.describes.me5(x$NFC11)
nfc12 <- l2num.describes.me5(x$NFC12R)
nfc13 <- l2num.describes.me5.reverse(x$NFC13R)
nfc14 <- l2num.describes.me5(x$NFC14)
nfc15 <- l2num.describes.me5(x$NFC15)
nfc16 <- l2num.describes.me5(x$NFC16)
nfc17 <- l2num.describes.me5.reverse(x$NFC17R)
nfc18 <- l2num.describes.me5.reverse(x$NFC18R)
  
nfc.scale <- data.frame(nfc1, nfc2, nfc3,
                        nfc4, nfc5, nfc6,
                        nfc7, nfc8, nfc9,
                        nfc10, nfc11, nfc12,
                        nfc13, nfc14, nfc15,
                        nfc16, nfc17, nfc18)

alpha(nfc.scale)

nfc.scale <- ((nfc1 + nfc2 + nfc3 +
               nfc4 + nfc5 + nfc6 +
               nfc7 + nfc8 + nfc9 +
               nfc10 + nfc11 + nfc12 +
               nfc13 + nfc14 + nfc15 +
               nfc16 + nfc17 + nfc18)/18)

#Condition scale construction
# Ohio = Clinton lead, Trump trail, NC = Trump lead, Clinton trail, Iowa = Tied
condition <- factor(x$Condition,
                    levels = c("Ohio",
                               "Iowa",
                               "North Carolina"))
condition <- as.numeric(condition)
condition

#HMP favorability scale

hmp.favor <- factor(x$hmp_condition_favor,
                    levels = c("Strongly favored Clinton",
                               "Mostly favored Clinton",
                               "Slightly favored Clinton",
                               "Article was strictly neutral",
                               "Slightly favored Trump",
                               "Mostly favored Trump",
                               "Strongly favored Trump"))


# HMP bias scale construction - Clinton
# 1 = Not at all biased, 5 = very biased against Clinton

l2num.bias.clinton5 <- function(y) {
  y <- factor(y,
              levels = c("Not at all biased against Clinton",
                         "Slightly biased against Clinton",
                         "Somewhat biased against Clinton",
                         "Mostly biased against Clinton",
                         "Very biased against Clinton"))
}

clinton.bias <- l2num.bias.clinton5(x$hmp_condition_clinton)


# HMP bias scale construction - Trump
# 1 = Not at all biased, 5 = very biased against Trump

l2num.bias.trump5 <- function(y) {
  y <- factor(y,
              levels = c("Not at all biased against Trump",
                         "Slightly biased against Trump",
                         "Somewhat biased against Trump",
                         "Mostly biased against Trump",
                         "Very biased against Trump"))
}

trump.bias <- l2num.bias.trump5(x$hmp_condition_trump)


# HMP bias scale construction - Journalist
# 1 = Strongly favored Clinton, 3 = midpoint, neutral, 5 = Strongly favored Trump
l2num.bias.journalist <- function(y) { 
  y <- factor(y,
              levels = c("Strongly favored Clinton",
                         "Mostly favored Clinton",
                         "Slightly favored Clinton",
                         "Journalist was strictly neutral",
                         "Slightly favored Trump",
                         "Mostly favored Trump",
                         "Strongly favored Trump"))
}

hmp.journalist <- l2num.bias.journalist(x$hmp_condition_journalist)

# Journalist mistrust scale construction
# 1 = strongly disagree, 5 = strongly agree
l2num.agree7 <- function(y) {
  y <- factor(y,
              levels = c("Strongly disagree",
                         "Disagree",
                         "Somewhat disagree",
                         "Neither agree nor disagree",
                         "Somewhat agree",
                         "Agree",
                         "Strongly agree"))
  as.numeric(y)
}

l2num.agree7.reverse <- function(y) {
  y <- factor(y,
              levels = c("Strongly agree",
                         "Agree",
                         "Somewhat agree",
                         "Neither agree nor disagree",
                         "Somewhat disagree",
                         "Disagree",
                         "Strongly disagree"))
  as.numeric(y)
}
                         

journ.clicks <- l2num.agree7(x$journ_clicks)
journ.personal <- l2num.agree7(x$journ_personal)
journ.pretend <- l2num.agree7(x$journ_pretend)
journ.rig <- l2num.agree7(x$journ_rig)
journ.twist <- l2num.agree7(x$journ_twist)
journ.info <- l2num.agree7.reverse(x$journ_infoR)
journ.objective <- l2num.agree7.reverse(x$journ_objectivityR)

journ.bias.scale <- data.frame(journ.clicks, 
                               journ.personal, 
                               journ.pretend, 
                               journ.rig, 
                               journ.twist,
                               journ.info,
                               journ.objective)
alpha(journ.bias.scale)

journ.bias.scale <- ((journ.clicks +
                      journ.personal +
                      journ.pretend +
                      journ.rig +
                      journ.twist +
                      journ.info +
                      journ.objective)/7)


#Candidate Support scale
cand.support <- factor(x$cand_support,
                       levels = c("Hillary Clinton",
                                  "Dontald Trump"))
cand.support <- as.numeric(cand.support)
cand.support

#Media Distrust scale
media.trust <- factor(x$media_trust,
                      levels = c("None of the time",
                                 "Only some of the time",
                                 "About half the time",
                                 "Most of the time",
                                 "All of the time"))

media.trust <- as.numeric(media.trust)
media.trust

media.distrust.scale <- data.frame(x$media_complete,
                                   x$media_untrustworthy,
                                   x$media_inaccurate,
                                   x$media_unfair)
alpha(media.distrust.scale)

media.distrust.scale <- ((x$media_complete + x$media_untrustworthy + 
                          x$media_inaccurate + x$media_unfair)/4)

#Prayer scale construction
# 1 = Never, 5 = Several times a day

prayer <- factor(x$pray,
                 levels = c("Never",
                            "Once a week or less",
                            "A few times a week",
                            "Once a day",
                            "Several times a day"))

prayer <- as.numeric(prayer)


#Religious fundamentalism scale construction
# 1 = strongly disagree, 5 = strongly agree


rel.fund1 <- l2num.agree5(x$rel_fund1)
rel.fund2 <- l2num.agree5(x$rel_fund2)
rel.fund3 <- l2num.agree5(x$rel_fund3)
rel.fund4 <- l2num.agree5(x$rel_fund4)

rel.fund.scale <- data.frame(rel.fund1, 
                             rel.fund2, 
                             rel.fund3, 
                             rel.fund4)
alpha(rel.fund.scale)

rel.fund.scale <- ((rel.fund1 + rel.fund2 + rel.fund3 + rel.fund4)/4)

#Effect on voter likelihood scales - Trump
# 1 = less likely to vote, 3 = more likely to vote
cat2num.trump.effect <- function(y) {
  y <- factor(y,
              levels = c("Articles like this one make Trump supporters less likely to vote",
                         "Articles like this one do not affect Trump supporters&#39; decision to vote",
                         "Articles like this one make Trump supporters more likely to vote"))
  
  as.numeric(y)
}

trump.effect.iowa <- cat2num.trump.effect(x$effect_trump_iowa)
trump.effect.nc <- cat2num.trump.effect(x$effect_trump_NC)
trump.effect.ohio <- cat2num.trump.effect(x$effect_trump_ohio)

#Effect on voter likelihood scales - Clinton
# 1 = less likely to vote, 3 = more likely to vote
cat2num.clinton.effect <- function(y) {
  y <- factor(y,
              levels = c("Articles like this one make Clinton supporters less likely to vote",
                         "Articles like this one do not affect Clinton supporters&#39; decision to vote",
                         "Articles like this one make Clinton supporters more likely to vote"))

  as.numeric(y)
}


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