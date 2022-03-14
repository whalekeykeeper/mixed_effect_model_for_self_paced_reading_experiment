library(dplyr)
library(lme4)

# read data
df = read.csv("https://raw.githubusercontent.com/whalekeykeeper/mixed_effect_model_for_self_paced_reading_experiment/main/data_processed.csv?token=GHSAT0AAAAAABP4G2PEW6SE5ZBJA3UYVTKIYRX4SOQ")
head(df)

# Trigger Sentence
select_trigger <- df %>%
  filter(type == "trigger")
head(select_trigger)

boxplot(avg_rt ~ full_or_partial*setting,
        col=c("white","lightgray"),select_trigger)


# H1: full-knowledge caused longer RT over the scalar quantifier than partial-knowledge
select_trigger_quantifier <- df %>%
  filter(type == "trigger", quantifier == "quantifier")
head(select_trigger_quantifier)

df.null = lmer(frequency ~ setting +
                         (1|submission_id) + (1|itemID), data=select_trigger_quantifier, REML=FALSE)
df.model = lmer(avg_rt ~ full_or_partial + setting +
                  (1|submission_id) + (1|itemID), data=select_trigger_quantifier, REML=FALSE)
summary(df.model)
anova(df.null, df.model)
coef(df.model)

