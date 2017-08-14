#clear environment
rm(list = ls())

#load packages
library(tidyverse)
library(psych)
library(gridExtra)

#load dataset
x <- read.csv("~/RStats/Magic HME data - clean.csv", header = TRUE, na.strings = c("", "NA"))

#Descriptives -- Education

edu.bar <- ggplot(x, aes(education)) +
           ggtitle("Distribution of Education") +
           geom_bar() +
           xlab("Education") +
           ylab("Total Number of People") +
           ylim(c(0, 400)) +
           scale_x_discrete(limits = c("Less than high school",
                                     "High school graduate",
                                     "Some college",
                                     "2 year degree",
                                     "4 year degree",
                                     "Postgraduate degree"))

edu.bar
edu.bar.condition <- edu.bar + facet_grid(Condition ~ .) +
                     ylim(c(0,150))
edu.bar.condition
#Descriptives -- Income
  
i.bar <- ggplot(x, aes(income)) +
         ggtitle("Distribution of Income") +
         xlab("Income") +
         theme(axis.text.x=element_text(angle=5)) +
         ylab("Total Number of People") +
         ylim(c(0, 250)) +
         geom_bar() +
         scale_x_discrete(limits = c("Less than $15,000",
                                     "$15,001-$30,000",
                                     "$30,001-$45,000",
                                     "$45,001-$60,000",
                                     "$60,001-$75,000",
                                     "$75,001-$100,000",
                                     "$100,001-$150,000",
                                     "$150,001 or more"))

i.bar
i.bar.condition <- i.bar + facet_grid(Condition ~ .) +
                   ylim(c(0, 100))
i.bar.condition

#Descriptives -- Sex

sex.bar <- ggplot(x, aes(sex)) +
  ggtitle("Distribution of Sex") +
  xlab("Sex") +
  ylab("Total Number of People") +
  geom_bar() +
  scale_x_discrete(limits = c("Female",
                              "Male",
                              "Other identity"))
sex.bar

sex.bar.condition <- sex.bar + facet_grid(Condition ~ .) +
                     ylim(c(0, 300))
sex.bar.condition

#Descriptives -- Age
describe(x$age)
age.bar <- ggplot(x, aes(age)) +
  ggtitle("Distribution of Age") +
  xlab("Age") +
  ylab("Total Number of People") +
  geom_bar()
  
age.bar

age.bar.condition <- age.bar + facet_grid(Condition ~ .)
age.bar.condition

#Descriptives -- Candidate Support
candsupport.bar <- ggplot(x, aes(cand_support)) +
  ggtitle("Distribution of Candidate Support") +
  xlab("Candidate") +
  ylab("Total Number of People Supporting") +
  ylim(c(0, 550)) +
  geom_bar() +
  scale_x_discrete(limits = c("Hillary Clinton",
                            "Donald Trump",
                            "Jill Stein",
                            "Gary Johnson",
                            "I do not support any of the candidates",
                            "I haven't decided yet"))
candsupport.bar

candsupport.bar.condition <- candsupport.bar + facet_grid(Condition ~ .) +
                             ylim(c(0, 200))
candsupport.bar.condition

#Descriptives -- Party ID

partyid.bar <- ggplot(x, aes(partyID)) +
  ggtitle("Distribution of Party ID") +
  xlab("Party ID") +
  ylab("Total Number of People") +
  ylim(c(0, 500)) +
  geom_bar() +
  scale_x_discrete(limits = c("Democrat",
                              "Independent",
                              "Republican",
                              "Something else"))

#Descriptives -- econ partisanship
describe(partisanship.econ)
partisan.econ.bar <- ggplot(x, aes(partisanship_econ)) +
  ggtitle("Distribution of Economic Partisanship") +
  xlab("Partisanship") +
  ylab("Total Number of People") +
  geom_bar() +
  scale_x_discrete(limits = c("Very liberal",
                              "Liberal",
                              "Somewhat liberal",
                              "Moderate",
                              "Somewhat conservative",
                              "Conservative",
                              "Very conservative"))
partisan.econ.bar


partisan.econ.bar.condition <- partisan.econ.bar + facet_grid(Condition ~ .) +
  ylim(c(0, 150))
partisan.econ.bar.condition

#Descriptives -- social partisanship
describe(partisanship.social)
partisan.social.bar <- ggplot(x, aes(partisanship_social)) +
  ggtitle("Distribution of Social Partisanship") +
  xlab("Partisanship") +
  ylab("Total Number of People") +
  geom_bar() +
  scale_x_discrete(limits = c("Very liberal",
                              "Liberal",
                              "Somewhat liberal",
                              "Moderate",
                              "Somewhat conservative",
                              "Conservative",
                              "Very conservative"))
partisan.social.bar


partisan.social.bar.condition <- partisan.social.bar + facet_grid(Condition ~ .) +
  ylim(c(0, 150))
partisan.social.bar.condition

#Descriptives -- Need for Cognition Scale
describe(nfc.scale)
nfc.bar <- ggplot(x, aes(nfc.scale)) +
  geom_bar() +
  ggtitle("Need For Cognition Scale")
nfc.bar

nfc.bar.condition <- nfc.bar + facet_grid(Condition ~ .)
nfc.bar.condition

#Descriptives -- Conspiracy Scale
consp.bar <- ggplot(x, aes(consp.scale)) +
  geom_bar() +
  ggtitle("Conspiracy Belief Scale")
consp.bar

consp.bar.condition <- consp.bar + facet_grid(Condition ~ .)
consp.bar.condition

describe(consp.scale)

#Descriptives -- Paranormal Beliefs Scale
para.bar <- ggplot(x, aes(para.scale)) +
  geom_bar() +
  ggtitle("Paranormal Belief Scale") +
  ylim(c(0, 200))
para.bar

para.bar.condition <- para.bar + facet_grid(Condition ~ .) + ylim(c(0, 80))
para.bar.condition

describe(para.scale)

#Descriptives -- Feeling Thermometer -- Clinton
describe(x$thermo_clinton)
thermo.clinton.bar <- ggplot(x, aes(thermo_clinton)) +
  geom_bar()
thermo.clinton.bar

describe(x$thermo_clinton[x$Condition == "Iowa"])
describe(x$thermo_clinton[x$Condition == "North Carolina"])
describe(x$thermo_clinton[x$Condition == "Ohio"])
thermo.clinton.bar.condition <- thermo.clinton.bar + facet_grid(Condition ~ .)
thermo.clinton.bar.condition

#Descriptives -- Feeling thermometer - Trump
describe(x$thermo_trump)
thermo.trump.bar <- ggplot(x, aes(thermo_trump)) +
  geom_bar() +
  ggtitle("Feeling Thermometer - Trump")
thermo.trump.bar

describe(x$thermo_trump[x$Condition == "Iowa"])
describe(x$thermo_trump[x$Condition == "North Carolina"])
describe(x$thermo_trump[x$Condition == "Ohio"])
thermo.trump.bar.condition <- thermo.trump.bar + facet_grid(Condition ~ .)
thermo.trump.bar.condition

#Descriptives -- Feeling Thermometer -- Dems
describe(x$thermo_dems)
thermo.dems.bar <- ggplot(x, aes(thermo_dems)) +
  geom_bar()
thermo.dems.bar

describe(x$thermo_dems[x$Condition == "Iowa"])
describe(x$thermo_dems[x$Condition == "North Carolina"])
describe(x$thermo_dems[x$Condition == "Ohio"])
thermo.dems.bar.condition <- thermo.dems.bar + facet_grid(Condition ~ .)
thermo.dems.bar.condition

#Descriptives -- Feeling Thermometer -- GOP
describe(x$thermo_gop)
thermo.gop.bar <- ggplot(x, aes(thermo_gop)) +
  geom_bar()
thermo.gop.bar

describe(x$thermo_gop[x$Condition == "Iowa"])
describe(x$thermo_gop[x$Condition == "North Carolina"])
describe(x$thermo_gop[x$Condition == "Ohio"])
thermo.gop.bar.condition <- thermo.gop.bar + facet_grid(Condition ~ .)
thermo.gop.bar.condition

#Descriptives -- Voting intention
summary(x$vote_intent)
voteinterest.bar <- ggplot(x, aes(vote_intent)) +
  geom_bar()
voteinterest.bar

#Descriptives -- Election interest

elec.interest.bar <- ggplot(x, aes(election_interest)) +
  geom_bar()
elec.interest.bar

elec.interest.bar.condition <- elec.interest.bar + facet_grid(Condition ~ .)
elec.interest.bar.condition 

#Descriptives -- Bible beliefs

bible.bar <- ggplot(x, aes(bible_literal)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle=3)) +
  ggtitle("Distribution of Bible Beliefs") +
  xlab("Bible belief") +
  scale_x_discrete(limits = c("The Bible is a book written by men and is not the Word of God.",
                              "The Bible is the Word of God but not everything in it should be taken literally, word for word.",
                              "The Bible is the actual Word of God and is to be taken literally word for word."))
  
bible.bar

bible.bar.condition <- bible.bar + facet_grid(Condition ~ .) +
  ylim(c(0, 200))
bible.bar.condition

#Descriptives -- Religious Service Attendance
rel.services.bar <- ggplot(x, aes(x$religion_services)) +
  geom_bar() +
  ggtitle("Religious Service Attendance") +
  xlab("Frequency of Attending Religious Services") +
  scale_x_discrete(limits = c("Never",
                              "A few times a year",
                              "Once or twice a month",
                              "Almost every week",
                              "Every week",
                              "More than once a week"))
rel.services.bar

rel.services.bar.condition <- rel.services.bar + facet_grid(Condition ~ .)
rel.services.bar.condition

#Descriptives -- Religious Guidance
rel.guide.bar <- ggplot(x, aes(x$religion_guide)) +
  geom_bar() +
  ggtitle("Religious Guidance") +
  xlab("Strength of Religious Guidance") +
  ylim(c(0, 450)) +
  scale_x_discrete(limits = c("None",
                              "Some",
                              "Quite a bit",
                              "A great deal"))
rel.guide.bar
  

rel.guide.bar.condition <- rel.guide.bar + facet_grid(Condition ~ .) +
  ylim(c(0, 200))
rel.guide.bar.condition

#Descriptives -- Perceived Effects

###Trump-Iowa (control)
effect.trump.iowa.bar <- ggplot(x, aes(x$effect_trump_iowa)) +
  geom_bar() +
  ggtitle("Perceived Voter Effect on Trump Supporters -- Iowa") +
  theme(axis.text.x=element_text(angle=3)) +
  ylim(c(0, 200)) +
  scale_x_discrete(limits = c("Articles like this one make Trump supporters less likely to vote",
                              "Articles like this one do not affect Trump supporters&#39; decision to vote",
                              "Articles like this one make Trump supporters more likely to vote"))
effect.trump.iowa.bar

###Clinton-Iowa (control)
effect.clinton.iowa.bar <- ggplot(x, aes(x$effect_clinton_iowa)) +
  geom_bar() +
  ggtitle("Perceived Voter Effect on Clinton Supporters -- Iowa") +
  theme(axis.text.x=element_text(angle=3)) +
  ylim(c(0, 200)) +
  scale_x_discrete(limits = c("Articles like this one make Clinton supporters less likely to vote",
                              "Articles like this one do not affect Clinton supporters&#39; decision to vote",
                              "Articles like this one make Clinton supporters more likely to vote"))
effect.clinton.iowa.bar

###Trump-North Carolina
effect.trump.nc.bar <- ggplot(x, aes(x$effect_trump_NC)) +
  geom_bar() +
  ggtitle("Perceived Voter Effect of Article on Trump Supporters -- North Carolina") +
  theme(axis.text.x=element_text(angle=3)) +
  ylim(c(0, 200)) +
  scale_x_discrete(limits = c("Articles like this one make Trump supporters less likely to vote",
                              "Articles like this one do not affect Trump supporters&#39; decision to vote",
                              "Articles like this one make Trump supporters more likely to vote"))
effect.trump.nc.bar

###Clinton-North Carolina
effect.clinton.nc.bar <- ggplot(x, aes(x$effect_clinton_NC)) +
  geom_bar() +
  ggtitle("Perceived Voter Effect on Clinton Supporters -- North Carolina") +
  theme(axis.text.x=element_text(angle=3)) +
  ylim(c(0, 200)) +
  scale_x_discrete(limits = c("Articles like this one make Clinton supporters less likely to vote",
                              "Articles like this one do not affect Clinton supporters&#39; decision to vote",
                              "Articles like this one make Clinton supporters more likely to vote"))
effect.clinton.nc.bar

###Trump-Ohio
effect.trump.ohio.bar <- ggplot(x, aes(x$effect_trump_ohio)) +
  geom_bar() +
  ggtitle("Perceived Voter Effect of Article on Trump Supporters -- Ohio") +
  theme(axis.text.x=element_text(angle=3)) +
  ylim(c(0, 200)) +
  scale_x_discrete(limits = c("Articles like this one make Trump supporters less likely to vote",
                              "Articles like this one do not affect Trump supporters&#39; decision to vote",
                              "Articles like this one make Trump supporters more likely to vote"))
effect.trump.ohio.bar

###Clinton-Ohio
effect.clinton.ohio.bar <- ggplot(x, aes(x$effect_clinton_ohio)) +
  geom_bar() +
  ggtitle("Perceived Voter Effect on Clinton Supporters -- Ohio") +
  theme(axis.text.x=element_text(angle=3)) +
  ylim(c(0, 200)) +
  scale_x_discrete(limits = c("Articles like this one make Clinton supporters less likely to vote",
                              "Articles like this one do not affect Clinton supporters&#39; decision to vote",
                              "Articles like this one make Clinton supporters more likely to vote"))
effect.clinton.ohio.bar

# Descriptives -- Perceived candidate favorability 
# 1 = Strongly Favor Clinton, 7 = Strongly Favor Trump

hmp.favor.bar <- ggplot(x, aes(x$hmp_condition_favor)) +
  geom_bar() +
  ggtitle("Perceived Candidate Favorability") +
  theme(axis.text.x=element_text(angle=5)) +
  scale_x_discrete(limits = c("Strongly favored Clinton",
                              "Mostly favored Clinton",
                              "Slightly favored Clinton",
                              "Article was strictly neutral",
                              "Slightly favored Trump",
                              "Mostly favored Trump",
                              "Strongly favored Trump"))
hmp.favor.bar

hmp.favor.bar.condition <- hmp.favor.bar + facet_grid(Condition ~ .)
hmp.favor.bar.condition                              

#Descriptives -- Perceived Bias
# 1 = Not at all biased.., 5 = Very biased against..

###Clinton
hmp.bias.clinton <- ggplot(x, aes(x$hmp_condition_clinton)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle=5)) +
  ggtitle("Perceived Bias Against Clinton")
  scale_x_discrete(limits = c("Not at all biased against Clinton",
                              "Slightly biased against Clinton",
                              "Somewhat biased against Clinton",
                              "Mostly biased against Clinton",
                              "Very biased against Clinton"))
hmp.bias.clinton

hmp.bias.clinton.condition <- hmp.bias.clinton + facet_grid(Condition ~ .)
hmp.bias.clinton.condition

###Trump
hmp.bias.trump <- ggplot(x, aes(x$hmp_condition_trump)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle=5)) +
  ggtitle("Perceived Bias Against Trump")
  scale_x_discrete(limits = c("Not at all biased against Trump",
                              "Slightly biased against Trump",
                              "Somewhat biased against Trump",
                              "Mostly biased against Trump",
                              "Very biased against Trump"))
hmp.bias.trump

hmp.bias.trump.condition <- hmp.bias.trump + facet_grid(Condition ~ .)
hmp.bias.trump.condition
                              
###Journalist
hmp.bias.journ <- ggplot(x, aes(x$hmp_condition_journalist)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle=6)) +
  ggtitle("Perceived Journalist Bias") +
  scale_x_discrete(limits = c("Strongly favored Clinton",
                            "Mostly favored Clinton",
                            "Slightly favored Clinton",
                            "Journalist was strictly neutral",
                            "Slightly favored Trump",
                            "Mostly favored Trump",
                            "Strongly favored Trump"))
hmp.bias.journ

hmp.bias.journ.condition <- hmp.bias.journ + facet_grid(Condition ~ .)
hmp.bias.journ.condition

###hmp-general Clinton
hmp.general.clinton.bar <- ggplot(x, aes(x$hmp_general_clinton)) +
  geom_bar() +
  ggtitle("Perceived Media Bias Against Clinton -- General") +
  theme(axis.text.x=element_text(angle=6)) +
  scale_x_discrete(limits = c("Not at all biased against Clinton",
                              "Slightly biased against Clinton",
                              "Somewhat biased against Clinton",
                              "Mostly biased against Clinton",
                              "Very biased against Clinton"))
  
hmp.general.clinton.bar

hmp.general.clinton.bar.condition <- hmp.general.clinton.bar + facet_grid(Condition ~ .)
hmp.general.clinton.bar.condition

###hmp-general Trump
hmp.general.trump <- ggplot(x, aes(x$hmp_condition_trump)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle=5)) +
  ggtitle("Perceived Media Bias Against Trump -- General") +
  scale_x_discrete(limits = c("Not at all biased against Trump",
                            "Slightly biased against Trump",
                            "Somewhat biased against Trump",
                            "Mostly biased against Trump",
                            "Very biased against Trump"))
hmp.general.trump

hmp.general.trump.condition <- hmp.general.trump + facet_grid(Condition ~ .)
hmp.general.trump.condition

#Descriptives -- Journalism Bias Scale
describe(journ.bias.scale)

journ.bias.bar <- ggplot(x, aes(journ.bias.scale)) +
  geom_bar() +
  ggtitle("Journalism Bias Scale") +
  xlab("Evil Journalist Scale") +
journ.bias.bar

journ.bias.bar.condition <- journ.bias.bar + facet_grid(Condition ~ consp4) +
  ylim(c(0, 20))


journ.bias.bar.condition

#Descriptives - Political leaning + Magical Thinking

#Pessimism x partyID x Condition
partyid.pess.bar <- ggplot(x, aes(pess.scale)) +
  geom_bar() +
  ggtitle("Pessimism x Party ID") +
  facet_grid(partyID ~ .) +
  ylim(c(0, 80))
partyid.pess.bar

partyid.pess.bar.condition <- partyid.pess.bar + facet_grid(partyID ~ Condition) +
  ylim(c(0, 40)) +
  ggtitle("Pessimism x Party ID x Condition")
partyid.pess.bar.condition

#Pessimism x Econ Partisanship x Condition
econpartisan.pess.bar <- ggplot(x, aes(pess.scale)) +
  geom_bar() +
  ggtitle("Pessimism x Econ Partisanship") +
  facet_grid(x$partisanship_econ ~ .) +
  ylim(c(0, 80))
econpartisan.pess.bar

econpartisan.pess.bar.condition <- econpartisan.pess.bar + facet_grid(x$partisanship_econ ~ Condition) +
  ylim(c(0, 30)) +
  ggtitle("Pessimism x Econ Partisanship x Condition")
econpartisan.pess.bar.condition

#Pessimism x Social partisanship x Condition
socialpartisan.pess.bar <- ggplot(x, aes(pess.scale)) +
  geom_bar() +
  ggtitle("Pessimism x Social Partisanship") +
  facet_grid(x$partisanship_econ ~ .) +
  ylim(c(0, 80))
socialpartisan.pess.bar

socialpartisan.pess.bar.condition <- socialpartisan.pess.bar + facet_grid(x$partisanship_social ~ Condition) +
  ylim(c(0, 30)) +
  ggtitle("Pessimism x Social Partisanship x Condition")
socialpartisan.pess.bar.condition


#Paranormal Beliefs x partyID x Condition
partyid.para.bar <- ggplot(x, aes(para.scale)) +
  geom_bar() +
  ggtitle("Paranormal Beliefs x Party ID") +
  facet_grid(partyID ~ .) +
  ylim(c(0, 100)) 
partyid.para.bar

partyid.para.bar.condition <- partyid.para.bar + facet_grid(partyID ~ Condition) +
  ylim(c(0, 30)) +
  ggtitle("Paranormal Beliefs x Party ID x Condition")
partyid.para.bar.condition

#Paranormal Beliefs x Econ Partisanship x Condition
econpartisan.para.bar <- ggplot(x, aes(para.scale)) +
  geom_bar() +
  ggtitle("Paranormal Beliefs x Econ Partisanship") +
  facet_grid(x$partisanship_econ ~ .) +
  ylim(c(0, 60))
econpartisan.para.bar

econpartisan.para.bar.condition <- econpartisan.para.bar + facet_grid(x$partisanship_econ ~ Condition) +
  ylim(c(0, 30)) +
  ggtitle("Paranormal Beliefs x Econ Partisanship x Condition")
econpartisan.para.bar.condition

#Paranormal Beliefs x Social Partisanship x Condition
socialpartisan.para.bar <- ggplot(x, aes(para.scale)) +
  geom_bar() +
  ggtitle("Paranormal Beliefs x Social Partisanship") +
  facet_grid(x$partisanship_econ ~ .) +
  ylim(c(0, 60))
socialpartisan.para.bar

socialpartisan.para.bar.condition <- socialpartisan.para.bar + facet_grid(x$partisanship_social ~ Condition) +
  ylim(c(0, 30)) +
  ggtitle("Paranormal Beliefs x Social Partisanship x Condition")
socialpartisan.para.bar.condition

#Conspiracy Beliefs x partyID x Condition
partyid.consp.bar <- ggplot(x, aes(consp.scale)) +
  geom_bar() +
  ggtitle("Conspiracy Beliefs x Party ID") +
  facet_grid(partyID ~ .) +
  ylim(c(0, 100))
partyid.consp.bar

partyid.consp.bar.condition <- partyid.consp.bar + facet_grid(partyID ~ Condition) +
  ylim(c(0, 30)) +
  ggtitle("Conspiracy Beliefs x Party ID x Condition")
partyid.consp.bar.condition

#Conspiracy Beliefs x Econ Partisanship x Condition
econpartisan.consp.bar <- ggplot(x, aes(consp.scale)) +
  geom_bar() +
  ggtitle("Conspiracy Beliefs x Econ Partisanship") +
  facet_grid(x$partisanship_econ ~ .) +
  ylim(c(0, 60))
econpartisan.consp.bar

econpartisan.consp.bar.condition <- econpartisan.consp.bar + facet_grid(x$partisanship_econ ~ Condition) +
  ylim(c(0, 20)) +
  ggtitle("Conspiracy Beliefs x Econ Partisanship x Condition")
econpartisan.consp.bar.condition

#Conspiracy Beliefs x Social Partisanship x Condition
socialpartisan.consp.bar <- ggplot(x, aes(consp.scale)) +
  geom_bar() +
  ggtitle("Conspiracy Beliefs x Social Partisanship") +
  facet_grid(x$partisanship_econ ~ .) +
  ylim(c(0, 60))
socialpartisan.consp.bar

socialpartisan.consp.bar.condition <- socialpartisan.consp.bar + facet_grid(x$partisanship_social ~ Condition) +
  ylim(c(0, 20)) +
  ggtitle("Conspiracy Beliefs x Social Partisanship x Condition")
socialpartisan.consp.bar.condition

#Religious Fundamentalism x party ID x Condition
partyid.relfund.bar <- ggplot(x, aes(consp.scale)) +
  geom_bar() +
  ggtitle("Religious Fundamentalism x Party ID") +
  facet_grid(partyID ~ .) +
  ylim(c(0, 100))
partyid.relfund.bar

partyid.relfund.bar.condition <- partyid.consp.bar + facet_grid(partyID ~ Condition) +
  ylim(c(0, 30)) +
  ggtitle("Religious Fundamentalism x Party ID x Condition")
partyid.relfund.bar.condition

#Religious Fundamentalism x Econ Partisanship x Condition
econpartisan.relfund.bar <- ggplot(x, aes(rel.fund.scale)) +
  geom_bar() +
  ggtitle("Religious Fundamentalism x Econ Partisanship") +
  facet_grid(x$partisanship_econ ~ .) +
  ylim(c(0, 60))
econpartisan.relfund.bar

econpartisan.relfund.bar.condition <- econpartisan.relfund.bar + facet_grid(x$partisanship_econ ~ Condition) +
  ylim(c(0, 20)) +
  ggtitle("Religious Fundamentalism x Econ Partisanship x Condition")
econpartisan.relfund.bar.condition

#Religious Fundamentalism x Social Partisanship x Condition
socialpartisan.relfund.bar <- ggplot(x, aes(rel.fund.scale)) +
  geom_bar() +
  ggtitle("Religious Fundamentalism x Social Partisanship") +
  facet_grid(x$partisanship_econ ~ .) +
  ylim(c(0, 60))
socialpartisan.relfund.bar

socialpartisan.relfund.bar.condition <- socialpartisan.relfund.bar + facet_grid(x$partisanship_social ~ Condition) +
  ylim(c(0, 20)) +
  ggtitle("Religious Fundamentalism x Social Partisanship x Condition")
socialpartisan.relfund.bar.condition


##Barchart of % perceived bias by condition

#calculating percentages
x.ohio.perc <- x.ohio %>% count(partyID = factor(partyID), hmp_condition_trump = factor(hmp_condition_trump)) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) * 100) %>%





#Perceived Bias Against Trump by Party ID
x.ohio.trump.bias <- factor(x.ohio$hmp_condition_trump,
                            levels = c("Very biased against Trump",
                                       "Mostly biased against Trump",
                                       "Somewhat biased against Trump",
                                       "Slightly biased against Trump",
                                       "Not at all biased against Trump"))
x.iowa.trump.bias <- factor(x.iowa$hmp_condition_trump,
                            levels = c("Very biased against Trump",
                                       "Mostly biased against Trump",
                                       "Somewhat biased against Trump",
                                       "Slightly biased against Trump",
                                       "Not at all biased against Trump"))
x.nc.trump.bias <- factor(x.nc$hmp_condition_trump,
                            levels = c("Very biased against Trump",
                                       "Mostly biased against Trump",
                                       "Somewhat biased against Trump",
                                       "Slightly biased against Trump",
                                       "Not at all biased against Trump"))


trump.bias.ohio.bar <- ggplot(data = x.ohio, aes(x = partyID, fill = x.ohio.trump.bias)) +
  geom_bar(position = "fill") +
  labs(title = "Percentage of Perceived Bias Against Trump",
       subtitle = "Condition: North Carolina (Trump Ahead)",
       caption = "n = 341",
       x = "Party ID", 
       y = "Percentage of Total") +
  scale_x_discrete(limits = c("Democrat",
                              "Independent",
                              "Republican")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title = "Perceived Bias"))

trump.bias.iowa.bar <- ggplot(data = x.iowa, aes(x.iowa$partyID, fill = x.iowa.trump.bias)) +
  geom_bar(position = "fill") +
  labs(title = "Percentage of Perceived Bias Against Trump",
       subtitle = "Condition: North Carolina (Trump Ahead)",
       caption = "n = 340",
       x = "Party ID", 
       y = "Percentage of Total") +
  scale_x_discrete(limits = c("Democrat",
                              "Independent",
                              "Republican")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title = "Perceived Bias"))

trump.bias.nc.bar <- ggplot(data = x.nc, aes(x.nc$partyID, fill = x.nc.trump.bias)) +
  geom_bar(position = "fill") +
  labs(title = "Percentage of Perceived Bias Against Trump",
       subtitle = "Condition: North Carolina (Trump Ahead)",
       caption = "n = 334",
       x = "Party ID", 
       y = "Percentage of Total") +
  scale_x_discrete(limits = c("Democrat",
                              "Independent",
                              "Republican")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title = "Perceived Bias"))

#Perceived Bias Against Clinton by Party ID
x.ohio.clinton.bias <- factor(x.ohio$hmp_condition_clinton,
                            levels = c("Very biased against Clinton",
                                       "Mostly biased against Clinton",
                                       "Somewhat biased against Clinton",
                                       "Slightly biased against Clinton",
                                       "Not at all biased against Clinton"))
x.iowa.clinton.bias <- factor(x.iowa$hmp_condition_clinton,
                            levels = c("Very biased against Clinton",
                                       "Mostly biased against Clinton",
                                       "Somewhat biased against Clinton",
                                       "Slightly biased against Clinton",
                                       "Not at all biased against Clinton"))
x.nc.clinton.bias <- factor(x.nc$hmp_condition_clinton,
                          levels = c("Very biased against Clinton",
                                     "Mostly biased against Clinton",
                                     "Somewhat biased against Clinton",
                                     "Slightly biased against Clinton",
                                     "Not at all biased against Clinton"))

clinton.bias.ohio.bar <- ggplot(data = x.ohio, aes(x.ohio$partyID, fill = x.ohio.clinton.bias)) +
  geom_bar(position = "fill") +
  labs(title = "Percentage of Perceived Bias Against Clinton",
       subtitle = "Condition: Ohio (Clinton Ahead)",
       caption = "n = 341",
       x = "Party ID", 
       y = "Percentage of Total") +
  scale_x_discrete(limits = c("Democrat",
                              "Independent",
                              "Republican")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title = "Perceived Bias"))

clinton.bias.iowa.bar <- ggplot(data = x.iowa, aes(x.iowa$partyID, fill = x.iowa.clinton.bias)) +
  geom_bar(position = "fill") +
  labs(title = "Percentage of Perceived Bias Against Clinton",
       subtitle = "Condition: Iowa (Tied)",
       caption = "n = 340",
       x = "Party ID", 
       y = "Percentage of Total") +
  scale_x_discrete(limits = c("Democrat",
                              "Independent",
                              "Republican")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title = "Perceived Bias"))

clinton.bias.nc.bar <- ggplot(data = x.nc, aes(x.nc$partyID, fill = x.nc.clinton.bias)) +
  geom_bar(position = "fill") +
  labs(title = "Percentage of Perceived Bias Against Clinton",
       subtitle = "Condition: North Carolina (Trump Ahead)",
       caption = "n = 334",
       x = "Party ID", 
       y = "Percentage of Total") +
  scale_x_discrete(limits = c("Democrat",
                              "Independent",
                              "Republican")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title = "Perceived Bias"))


#Print Perceived Bias Charts
trump.bias.ohio.bar
trump.bias.iowa.bar
trump.bias.nc.bar
clinton.bias.ohio.bar
clinton.bias.iowa.bar
clinton.bias.nc.bar

#Perceived voter effect transformation
x.vote.effect.clinton <- factor(vote.effect.clinton,
                              levels = c("More likely to vote",
                                         "No effect on supporters",
                                         "Less likely to vote"))

x.vote.effect.trump <- factor(vote.effect.trump,
                              levels = c("More likely to vote",
                                         "No effect on voters",
                                         "Less likely to vote"))

#Voter Effect by Condition - Clinton Supporters
voter.effect.clinton.condition.bar <- ggplot(data = x, aes(x$Condition, fill = x.vote.effect.clinton)) +
  geom_bar(position = "fill") +
  labs(title = "Proportional Voter Effect By Condition",
       subtitle = "Clinton Supporters",
       x = "Condition", 
       y = "Percentage of Total") +
       scale_x_discrete(limits = c("Ohio",
                                   "Iowa",
                                   "North Carolina")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title = "Perceived Voter Effect"))

#Voter Effect by Condition - Trump supporters
voter.effect.trump.condition.bar <- ggplot(data = x, aes(x$Condition, fill = x.vote.effect.trump)) +
  geom_bar(position = "fill") +
  labs(title = "Proportional Voter Effect By Condition",
       subtitle = "Trump Supporters",
       x = "Condition", 
       y = "Percentage of Total") +
  scale_x_discrete(limits = c("Ohio",
                              "Iowa",
                              "North Carolina")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title = "Perceived Voter Effect"))

#Print Perceived Voter Effect By Condition Charts
voter.effect.clinton.condition.bar
voter.effect.trump.condition.bar

#Voter Effect by Party ID - Clinton Supporters
voter.effect.clinton.partyID.bar <- ggplot(data = x, aes(x$partyID, fill = x.vote.effect.clinton)) +
  geom_bar(position = "fill") +
  labs(title = "Proportional Voter Effect By Party ID",
       subtitle = "Clinton Supporters",
       x = "Party ID", 
       y = "Percentage of Total") +
  scale_x_discrete(limits = c("Democrat",
                              "Independent",
                              "Republican")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title = "Perceived Voter Effect"))

voter.effect.clinton.partyID.bar

#Voter Effect Trump
voter.effect.trump.partyID.bar <- ggplot(data = x, aes(x$partyID, fill = x.vote.effect.trump)) +
  geom_bar(position = "fill") +
  labs(title = "Proportional Voter Effect By Party ID",
       subtitle = "Trump Supporters",
       x = "Party ID", 
       y = "Percentage of Total") +
  scale_x_discrete(limits = c("Democrat",
                              "Independent",
                              "Republican")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title = "Perceived Voter Effect"))

#Print Perceived Voter Effect by Party ID Charts
voter.effect.clinton.partyID.bar
voter.effect.trump.partyID.bar

#Perceived voter effect by 