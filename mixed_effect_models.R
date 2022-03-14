library(dplyr)
library(lme4)

# read data
df = read.csv("https://raw.githubusercontent.com/whalekeykeeper/mixed_effect_model_for_self_paced_reading_experiment/main/data_processed.csv?token=GHSAT0AAAAAABP4G2PEASTWMDTM7IOBOYDGYRX7N7A")

# Log-transform data
df$avg_rt=log(df$avg_rt)
head(df)

# Trigger Sentence
select_trigger <- df %>%
  filter(type == "trigger")
head(select_trigger)

boxplot(avg_rt ~ full_or_partial*setting,
        col=c("white","lightgray"),select_trigger)

# H1: full-knowledge caused longer RT over the scalar quantifier of scalar trigger sentences than partial-knowledge
select_scalar_quantifier <- df %>%
  filter(setting == "scalar", quantifier == "quantifier")
head(select_scalar_quantifier)

boxplot(avg_rt ~ full_or_partial,
        col=c("white"),select_scalar_quantifier) # The plot does not support this H1 :(

df.null = lmer(avg_rt ~ 1 +
                 (1|submission_id) + (1|itemID), data=select_scalar_quantifier, REML=FALSE)
df.model = lmer(avg_rt ~ full_or_partial +
                  (1|submission_id) + (1|itemID), data=select_scalar_quantifier, REML=FALSE)
summary(df.model)
anova(df.null, df.model)
coef(df.model)

# H2: full-knowledge caused shorter RT over the anaphor part in complement sentences than partial-knowledge
select_complement_anaphor <- df %>%
  filter(setting == "complement", region == "c_anaphor")
head(select_complement_anaphor)

boxplot(avg_rt ~ full_or_partial,
        col=c("white"),select_complement_anaphor) # The plot does not support this H2 :(

df.null = lmer(avg_rt ~ 1 +
                 (1|submission_id) + (1|itemID), data=select_complement_anaphor, REML=FALSE)
df.model = lmer(avg_rt ~ full_or_partial +
                  (1|submission_id) + (1|itemID), data=select_complement_anaphor, REML=FALSE)
summary(df.model)
anova(df.null, df.model)  # It shows that the full-knowledge relates to shorter RT which does not support this H1
coef(df.model) 


# H3: full-knowledge show no significant different effect than partial-knowledge in focused trigger sentence 
select_focused_particle <- df %>%
  filter(setting == "focused", region == "f_particle")
head(select_complement_anaphor)

boxplot(avg_rt ~ full_or_partial,
        col=c("white"),select_focused_particle) # The differences between two means are very similar

df.null = lmer(avg_rt ~ 1 +
                 (1|submission_id) + (1|itemID), data=select_focused_particle, REML=FALSE)
df.model = lmer(avg_rt ~ full_or_partial +
                  (1|submission_id) + (1|itemID), data=select_focused_particle, REML=FALSE)
summary(df.model)
anova(df.null, df.model)  # Differences are small but the p-value is not significant
coef(df.model) 


# H4: No matter full-knowledge or partial knowledge, for scalar trigger, 
# RTs over all the regions are similar in complement sentences and cancellation sentences
select_continuation <- df %>%
  group_by(itemID)

head(select_continuation)

boxplot(avg_rt ~ full_or_partial,
        col=c("white"),select_continuation) # The differences between two means are very similar

df.null = lmer(avg_rt ~ 1 +
                 (1|submission_id) + (1|itemID), data=select_continuation, REML=FALSE)
df.model = lmer(avg_rt ~ setting +
                  (1|submission_id) + (1|itemID), data=select_continuation, REML=FALSE)
summary(df.model)
anova(df.null, df.model)  # Differences are small but the p-value is not significant
coef(df.model) 


# Other explores
