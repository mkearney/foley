require(Hmisc)

#Random correlation tables/factor analyses -- ignore -- work in progress
x1 <- data.frame(pess.war, pess.ebola, pess.recession, pess.terror, symb1, symb2, symb3,
                 symb4, symb5, symb6, consp1, consp2, consp3, consp4, para1, para2, para3,
                 para4, rel.fund1, rel.fund2, rel.fund3, rel.fund4)

x4 <- data.frame(consp.scale, para.scale, rel.fund.scale, pess.scale, symb.scale, appr.scale.fail)
round(cor(x1, use = "complete.obs"), 2)

out <- factanal(na.omit(x4), 3, rotation = "varimax")
print(out)

out1 <- rcorr(as.matrix(x4), 2)
out1

x6 <- data.frame(consp.scale, pess.scale, para.scale, rel.fund.scale, symb.scale, appr.scale.fail)
out6 <- round(cor(na.omit(x6)), 2)
out6


#Linear regression models for all outcome variables w/ all transformed variables - still
#need to include the race variable, still working on that one...

#HMP favorability - kitchen sink
model.hmp.favor <- lm(hmp.favor ~ x$age + sex + race + income + edu + bf.scale.agree +
                        bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                        bf.scale.open + nfc.scale + para.scale + consp.scale + 
                        rel.fund.scale + pess.scale + appr.scale.fail + symb.scale + rel.services + prayer + 
                        election.interest + election.news.attn + journ.bias.scale + 
                        media.distrust.scale + partisanship.scale +
                        partyID + condition)
summary(model.hmp.favor)

#HMP favorability - drop prayer
model.hmp.favor <- lm(hmp.favor ~ x$age + sex + race + income + edu + bf.scale.agree +
                        bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                        bf.scale.open + nfc.scale + para.scale + consp.scale + 
                        rel.fund.scale + pess.scale + symb.scale +  appr.scale.fail + rel.services +
                        election.interest + election.news.attn + journ.bias.scale + 
                        media.distrust.scale + partisanship.econ + partisanship.social +
                        partyID + condition)
summary(model.hmp.favor)


#HMP favorability - drop partisanship scales
model.hmp.favor <- lm(hmp.favor ~ x$age + sex + race + income + edu + bf.scale.agree +
                        bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                        bf.scale.open + nfc.scale + para.scale + consp.scale + 
                        rel.fund.scale + pess.scale + symb.scale +  appr.scale.fail + prayer + 
                        election.interest + election.news.attn + journ.bias.scale + 
                        media.distrust.scale + partyID + condition)
summary(model.hmp.favor)

#HMP favorability - drop partyID keep partisanship
model.hmp.favor <- lm(hmp.favor ~ x$age + sex + race + income + edu + bf.scale.agree +
                        bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                        bf.scale.open + nfc.scale + para.scale + consp.scale + 
                        rel.fund.scale + pess.scale + symb.scale +  appr.scale.fail + prayer + 
                        election.interest + election.news.attn + journ.bias.scale + 
                        media.distrust.scale + partisanship.econ + partisanship.social +
                        condition)
summary(model.hmp.favor)

#HMP Journalist
model.hmp.journalist <- lm(hmp.journalist ~ x$age + sex + race + income + edu + bf.scale.agree +
                             bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                             bf.scale.open + nfc.scale + para.scale + consp.scale + 
                             rel.fund.scale + pess.scale + symb.scale + appr.scale.fail + rel.services + prayer + 
                             election.interest + election.news.attn + journ.bias.scale + 
                             media.distrust.scale + partisanship.econ + partisanship.social +
                             partyID + condition)

summary(model.hmp.journalist)

#Perceived Bias Against Trump - kitchen sink
model.trump.bias <- lm(trump.bias ~ x$age + sex + race + income + edu + bf.scale.agree +
                         bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                         bf.scale.open + nfc.scale + para.scale + consp.scale + 
                         rel.fund.scale + pess.scale + symb.scale + appr.scale.fail + rel.services + prayer + 
                         election.interest + election.news.attn + journ.bias.scale + 
                         media.distrust.scale + partisanship.econ + partisanship.social +
                         partyID + condition)
summary(model.trump.bias)

#Perceived Bias Against Trump - minus prayer, partisanship
model.trump.bias <- lm(trump.bias ~ x$age + sex + race + income + edu + bf.scale.agree +
                         bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                         bf.scale.open + nfc.scale + para.scale + consp.scale + 
                         rel.fund.scale + pess.scale + symb.scale + appr.scale.fail + rel.services +
                         election.interest + election.news.attn + journ.bias.scale + 
                         media.distrust.scale + partyID)
summary(model.trump.bias)


#Perceived Bias Against Clinton - kitchen sink
model.clinton.bias <- lm(clinton.bias ~ x$age + sex + income + race + edu + bf.scale.agree +
                           bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                           bf.scale.open + nfc.scale + para.scale + consp.scale + 
                           rel.fund.scale + pess.scale + symb.scale + appr.scale.fail + rel.services + prayer + 
                           election.interest + election.news.attn + journ.bias.scale + 
                           media.distrust.scale + partisanship.econ + partisanship.social +
                           partyID + condition)

summary(model.clinton.bias)

#Perceived Bias Against Clinton - minus prayer, partisanship
model.clinton.bias <- lm(clinton.bias ~ x$age + sex + income + race + edu + bf.scale.agree +
                           bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                           bf.scale.open + nfc.scale + para.scale + consp.scale + 
                           rel.fund.scale + pess.scale + symb.scale + appr.scale.fail + rel.services + 
                           election.interest + election.news.attn + journ.bias.scale + 
                           media.distrust.scale + partyID + condition)
summary(model.clinton.bias)

#Perceived voter effects

#Ohio - Clinton ahead in poll
model.trump.effect.ohio <- lm(trump.effect.ohio ~ x$age + sex + race + income + edu + bf.scale.agree +
                                bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                                bf.scale.open + nfc.scale + para.scale + 
                                rel.fund.scale + pess.scale + symb.scale + appr.scale.fail + rel.services + prayer + 
                                election.interest + election.news.attn + journ.bias.scale + 
                                media.distrust.scale + partisanship.econ + partisanship.social +
                                partyID, 
                              data = subset(x, condition == 1))

model.effect.trump.iowa
