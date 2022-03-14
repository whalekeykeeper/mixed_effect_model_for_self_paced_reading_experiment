library(dplyr)
library(lme4)

# read data
df = read.csv("https://raw.githubusercontent.com/whalekeykeeper/mixed_effect_model_for_self_paced_reading_experiment/main/data_processed.csv?token=GHSAT0AAAAAABP4G2PFOSIHFSNYUWBYLI7KYRX75MA")

# Log-transform data
df$avg_rt=log(df$avg_rt)
head(df)

# Trigger Sentence
select_trigger <- df %>%
  filter(type == "trigger")
head(select_trigger)

boxplot(avg_rt ~ full_or_partial*setting,
        col=c("white","lightblue"),select_trigger)

# H1: full-knowledge caused longer RT over the scalar quantifier of scalar trigger sentences than partial-knowledge
select_scalar_quantifier <- df %>%
  filter(setting == "scalar", quantifier == "quantifier")
head(select_scalar_quantifier)

boxplot(avg_rt ~ full_or_partial,
        col=c("lightblue"),select_scalar_quantifier) # The plot does not support this H1 :(

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
        col=c("lightblue"),select_complement_anaphor) # The plot does not support this H2 :(

df.null = lmer(avg_rt ~ 1 +
                 (1|submission_id) + (1|itemID), data=select_complement_anaphor, REML=FALSE)
df.model = lmer(avg_rt ~ full_or_partial +
                  (1|submission_id) + (1|itemID), data=select_complement_anaphor, REML=FALSE)
summary(df.model)
anova(df.null, df.model)  # It shows that the full-knowledge relates to shorter RT which does not support this H1
coef(df.model) 


# H3: full-knowledge show no significant different effect over the "focus particle" than partial-knowledge in focused trigger sentence 
select_focused_particle <- df %>%
  filter(setting == "focused", region == "f_particle")
head(select_complement_anaphor)

boxplot(avg_rt ~ full_or_partial,
        col=c("lightblue"),select_focused_particle) # The differences between two means are very similar

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
  filter(type == "continuation", scalar_in_item_or_not == "scalar")
head(select_continuation)

boxplot(avg_rt ~ full_or_partial,
        col=c("lightblue"),select_continuation) # The differences between two means are very similar

df.null = lmer(avg_rt ~ 1 +
                 (1|submission_id) + (1|itemID), data=select_continuation, REML=FALSE)
df.model = lmer(avg_rt ~ full_or_partial +
                  (1|submission_id) + (1|itemID), data=select_continuation, REML=FALSE)
summary(df.model)
anova(df.null, df.model)  # The p-value is still not significant but much better than earlier ones
coef(df.model) 


# Other explores
# Check the RTs of predicate in Complement Sentences
select_cmplement_predicate <- df %>%
  filter(setting == "complement", region == "predicate")
head(select_cmplement_predicate)

boxplot(avg_rt ~ full_or_partial*scalar_in_item_or_not,
        col=c("white","lightblue"),select_cmplement_predicate)
# The picture seems to predict that: 
#for "scalar trigger", knowledge doesn't matter;
#but for "focused trigger", full-knowledge increased the RTs than partial-knowledge
#which is slightly different than H3
# H3 says that for "focused particle" part that the knowledge background should not change anything


df.model = lmer(avg_rt ~ full_or_partial + scalar_in_item_or_not +
                  (1|submission_id) + (1|itemID), data=select_cmplement_predicate, REML=FALSE)
summary(df.model)

df.model.interaction = lmer(avg_rt ~ full_or_partial*scalar_in_item_or_not +
                (1|submission_id) + (1|itemID), data=select_cmplement_predicate, REML=FALSE)
summary(df.model.interaction) # The Chisq value is 0.0136

anova(df.model, df.model.interaction)
coef(df.model.interaction) 

