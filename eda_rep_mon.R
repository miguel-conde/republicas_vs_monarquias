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
