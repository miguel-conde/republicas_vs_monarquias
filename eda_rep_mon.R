library(tidyverse)

library(readr)
all_data <- read_delim("data/rep_mon.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)

# EDA ---------------------------------------------------------------------

aov_all <- aov(int_dollars ~ regime_type*const_form, data = all_data)

summary(aov_all)

tukey_aov <- TukeyHSD(aov_all)
tukey_aov

par(mfrow = c(1, 2))
plot(tukey_aov)
par(mfrow = c(1, 1))


# ANALYSIS ----------------------------------------------------------------

## 
ftable <- table(all_data$regime_type, all_data$const_form)
mosaicplot(ftable)
chisq.test(ftable)

##
boxplot(score ~ const_form, all_data)
oneway.test(score ~ const_form, all_data)
all_data_aov <- aov(score ~ const_form, all_data)
summary(all_data_aov)
model.tables(all_data_aov, "means")
all_data_post_hoc <- TukeyHSD(all_data_aov)
all_data_post_hoc
plot(all_data_post_hoc)

##
interaction.plot(all_data$regime_type, all_data$const_form, all_data$int_dollars, type = "b")


##
lm_wealthness <- lm(int_dollars ~ score + score_ief + const_form - 1,
                    all_data)
summary(lm_wealthness)

lm_equality <- lm(gini_index ~   score_ief + const_form  - 1,
                    all_data)
summary(lm_equality)


library(lme4)

hlm_wealthness <- lmer(int_dollars ~ score_ief + gini_index - 1 +
                       (score_ief + gini_index | const_form) +
                       (score_ief + gini_index| regime_type),
                     all_data)
summary(hlm_wealthness)
coef(hlm_wealthness)
fixef(hlm_wealthness)
ranef(hlm_wealthness)

nlm_wealthness <- lmer(int_dollars ~ score_ief + gini_index - 1 +
                         (score_ief + gini_index | const_form/regime_type), 
                       all_data)
summary(nlm_wealthness)
coef(nlm_wealthness)
fixef(nlm_wealthness)
ranef(nlm_wealthness)

hlm_equality <- lmer(gini_index ~ score_ief + int_dollars - 1 +
                         (score_ief + int_dollars  | const_form) +
                         (score_ief + int_dollars | regime_type),
                       all_data)
summary(hlm_equality)
coef(hlm_equality)
fixef(hlm_equality)
ranef(hlm_equality)

nlm_equality <- lmer(gini_index ~ score_ief + int_dollars - 1 +
                         (score_ief + int_dollars | const_form/regime_type), 
                       all_data)
summary(nlm_equality)
coef(nlm_equality)
fixef(nlm_equality)
ranef(nlm_equality)
