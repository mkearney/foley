require(MASS)
require(xtable)

#Perceived Bias Against Clinton - kitchen sink
model.clinton.bias <- polr(clinton.bias ~ x$age + sex + income + white + edu + bf.scale.agree +
                           bf.scale.consc + bf.scale.neuro +
                           bf.scale.open + nfc.scale + para.scale + consp.scale + rel.fund.scale + pess.scale + prayer +
                           election.interest + election.news.attn + journ.bias.scale + 
                           media.distrust.scale + strong.partisan
                           partyID.scale + condition,
                           method = "logistic")

summary(model.clinton.bias)

coef <- c(model.clinton.bias$coefficients, model.clinton.bias$zeta)
se <- sqrt(diag(vcov(model.clinton.bias)))
z <- coef/se
p <- 2 * (1 - pnorm(abs(z)))
xtable(cbind(coef, se, z, p), digits = 7)

exp(-model.clinton.bias$coefficients)
100 * (exp(-model.clinton.bias$coefficients)-1)


model.clinton.bias <- polr(clinton.bias ~ scale(x$age) + scale(sex) + scale(income) + scale(white) + scale(edu) + scale(bf.scale.agree) +
                             scale(bf.scale.consc) + scale(bf.scale.neuro) +
                             scale(bf.scale.open) + scale(nfc.scale) + scale(para.scale) + scale(consp.scale) + 
                             scale(rel.fund.scale) + scale(appr.scale.fail) + scale(symb.scale) +
                             scale(pess.scale) + scale(prayer) + scale(election.interest) + scale(election.news.attn) + 
                             scale(journ.bias.scale) + scale(media.distrust.scale) + scale(strong.partisan) +
                             scale(partyID.scale) + scale(condition),
                           method = "logistic")

summary(model.clinton.bias)


coef <- c(model.clinton.bias$coefficients, model.clinton.bias$zeta)
se <- sqrt(diag(vcov(model.clinton.bias)))
z <- coef/se
p <- 2 * (1 - pnorm(abs(z)))
xtable(cbind(coef, se, z, p), digits = 7)

exp(-model.clinton.bias$coefficients)
100 * (exp(-model.clinton.bias$coefficients)-1)

#Trump Bias
model.trump.bias <- polr(trump.bias ~ scale(x$age) + scale(sex) + scale(income) + scale(white) + scale(edu) + scale(bf.scale.agree) +
                             scale(bf.scale.consc) + scale(bf.scale.neuro) +
                             scale(bf.scale.open) + scale(nfc.scale) + scale(para.scale) + scale(consp.scale) + 
                             scale(rel.fund.scale) + 
                             scale(pess.scale) + scale(prayer) + scale(election.interest) + scale(election.news.attn) + 
                             scale(journ.bias.scale) + scale(media.distrust.scale) + scale(strong.partisan) +
                             scale(partyID.scale) + scale(condition),
                           method = "logistic")

summary(model.trump.bias)

coef <- c(model.trump.bias$coefficients, model.trump.bias$zeta)
se <- sqrt(diag(vcov(model.trump.bias)))
z <- coef/se
p <- 2 * (1 - pnorm(abs(z)))
xtable(cbind(coef, se, z, p), digits = 7)

exp(-model.trump.bias$coefficients)
100 * (exp(-model.trump.bias$coefficients)-1)

ci <- confint(model.trump.bias)
ci

#HMP favorability - kitchen sink - Linear
model.hmp.favor <- lm(hmp.favor ~ x$age + sex + race + income + edu + bf.scale.agree +
                        bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                        bf.scale.open + nfc.scale + para.scale + consp.scale + 
                        rel.fund.scale + pess.scale + appr.scale.fail + symb.scale + rel.services + prayer + 
                        election.interest + election.news.attn + journ.bias.scale + 
                        media.distrust.scale + partisanship.scale +
                        partyID.scale + strong.partisan + condition)
summary(model.hmp.favor)

#HMP favorability - Ordinal Logit Regression - minus apprehension scale + symbolic thinking scale
model.hmp.favor <- polr(hmp.favor ~ x$age + sex + white + income + edu + bf.scale.agree +
                        bf.scale.consc + bf.scale.extro + bf.scale.neuro +
                        bf.scale.open + nfc.scale + para.scale + consp.scale + 
                        rel.fund.scale + pess.scale + rel.services + prayer +
                        election.interest + election.news.attn + journ.bias.scale + 
                        media.distrust.scale + strong.partisan + partisanship.scale +
                        partyID.scale + condition,
                        method = "logistic")
summary(model.hmp.favor)
coef <- c(model.hmp.favor$coefficients, model.hmp.favor$zeta)
se <- sqrt(diag(vcov(model.hmp.favor)))
z <- coef/se
p <- 2 * (1 - pnorm(abs(z)))
xtable(cbind(coef, se, z, p), digits = 7)

exp(-model.hmp.favor$coefficients)
100 * (exp(-model.hmp.favor$coefficients)-1)

#Perceived Effect on Voters

#Clinton
model.vote.effect.clinton <- polr(vote.effect.clinton ~ scale(x$age) + scale(sex) + scale(income) + scale(white) + scale(edu) + scale(bf.scale.agree) +
                             scale(bf.scale.consc) + scale(bf.scale.neuro) +
                             scale(bf.scale.open) + scale(nfc.scale) + scale(para.scale) + scale(consp.scale) + 
                             scale(rel.fund.scale) + scale(appr.scale.fail) + scale(symb.scale) +
                             scale(pess.scale) + scale(prayer) + scale(election.interest) + scale(election.news.attn) + 
                             scale(journ.bias.scale) + scale(media.distrust.scale) + scale(strong.partisan) +
                             scale(partyID.scale) + scale(condition),
                           method = "logistic")

summary(model.vote.effect.clinton)



coef <- c(model.vote.effect.clinton$coefficients, model.vote.effect.clinton$zeta)
se <- sqrt(diag(vcov(model.vote.effect.clinton)))
z <- coef/se
p <- 2 * (1 - pnorm(abs(z)))
xtable(cbind(coef, se, z, p), digits = 7)

exp(-model.vote.effect.clinton$coefficients)
100 * (exp(-model.vote.effect.clinton$coefficients)-1)

#Trump
model.vote.effect.trump <- polr(vote.effect.trump ~ scale(x$age) + scale(sex) + scale(income) + scale(white) + scale(edu) + scale(bf.scale.agree) +
                                    scale(bf.scale.consc) + scale(bf.scale.neuro) +
                                    scale(bf.scale.open) + scale(nfc.scale) + scale(para.scale) + scale(consp.scale) + 
                                    scale(rel.fund.scale) + scale(appr.scale.fail) + scale(symb.scale) +
                                    scale(pess.scale) + scale(prayer) + scale(election.interest) + scale(election.news.attn) + 
                                    scale(journ.bias.scale) + scale(media.distrust.scale) + scale(strong.partisan) +
                                    scale(partyID.scale) + scale(condition),
                                  method = "logistic")

summary(model.vote.effect.trump)



coef <- c(model.vote.effect.trump$coefficients, model.vote.effect.trump$zeta)
se <- sqrt(diag(vcov(model.vote.effect.trump)))
z <- coef/se
p <- 2 * (1 - pnorm(abs(z)))
xtable(cbind(coef, se, z, p), digits = 7)

exp(-model.vote.effect.trump$coefficients)
100 * (exp(-model.vote.effect.trump$coefficients)-1)
