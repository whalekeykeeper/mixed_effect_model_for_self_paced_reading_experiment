library(dplyr)
library(lme4)

# read data
df = read.csv("https://raw.githubusercontent.com/whalekeykeeper/mixed_effect_model_for_self_paced_reading_experiment/main/data_processed.csv")



# Log-transform data
df$avg_rt=log(df$avg_rt)
head(df)



# Create a simple linear regression model and use residual plots to check assumptions for a linear regression model
xmdl= lm(avg_rt ~ full_or_partial, df)
summary(xmdl)
plot(fitted(xmdl), residuals(xmdl)) # This one looks a bit weird
hist(residuals(xmdl)) # Left-skewed
qqnorm(residuals(xmdl)) # Looks okay
dfbeta(xmdl)

# Trigger Sentences
# Full data
# Quantifier: S/some of
data <- df %>%
  filter(type == "trigger", region == "s_quantifier" | region == "f_quantifier")
head(data)

boxplot(avg_rt ~ full_or_partial,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.participants = lmer(avg_rt ~ full_or_partial*scalar_or_focused +
                  (1|submission_id), data=data, REML=FALSE)
# Items
model.items = lmer(avg_rt ~ full_or_partial*scalar_or_focused +
                               (1|itemID), data=data, REML=FALSE)
summary(model.participants)
summary(model.items)
anova(model.participants, model.items)
coef(model.participants)
coef(model.items)
# Plots
plot(fitted(model.participants), residuals(model.participants))
hist(residuals(model.participants)) 
qqnorm(residuals(model.participants))


# Trigger Sentences
# Full data
# Region 2: the | real-estate
data <- df %>%
  filter(type == "trigger", region == "s_quantifier" | region == "f_quantifier")
head(data)

boxplot(avg_rt ~ full_or_partial,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.participants = lmer(avg_rt ~ full_or_partial*scalar_or_focused +
                            (1|submission_id), data=data, REML=FALSE)
# Items
model.items = lmer(avg_rt ~ full_or_partial*scalar_or_focused +
                     (1|itemID), data=data, REML=FALSE)
summary(model.participants)
summary(model.items)
anova(model.participants, model.items)
coef(model.participants)
coef(model.items)
# Plots
plot(fitted(model.participants), residuals(model.participants))
hist(residuals(model.participants)) 
qqnorm(residuals(model.participants)) 
























# 
# 
# # H1: full-knowledge caused longer RT over the scalar quantifier of scalar trigger sentences than partial-knowledge
# select_scalar_quantifier <- df %>%
#   filter(setting == "scalar", quantifier == "quantifier")
# head(select_scalar_quantifier)
# 
# boxplot(avg_rt ~ full_or_partial,
#         col=c("lightblue"),select_scalar_quantifier) # The plot does not support this H1 :(
# 
# h1.null = lmer(avg_rt ~ 1 +
#                  (1|submission_id) + (1|itemID), data=select_scalar_quantifier, REML=FALSE)
# h1.model = lmer(avg_rt ~ full_or_partial +
#                   (1|submission_id) + (1|itemID), data=select_scalar_quantifier, REML=FALSE)
# summary(h1.model)
# anova(h1.null, h1.model)
# coef(h1.model)
# # Plots for checking violation of linear assumptions
# plot(fitted(h1.model), residuals(h1.model)) # This one is a bit weird
# hist(residuals(h1.model)) # Looks not that normal-distributed on the right part
# qqnorm(residuals(h1.model)) # Looks okay
# 
# 
# 
# # H2: full-knowledge caused shorter RT over the anaphor part in complement sentences than partial-knowledge
# select_complement_anaphor <- df %>%
#   filter(setting == "complement", region == "c_anaphor")
# head(select_complement_anaphor)
# 
# boxplot(avg_rt ~ full_or_partial,
#         col=c("lightblue"),select_complement_anaphor) # The plot does not support this H2 :(
# 
# h2.null = lmer(avg_rt ~ 1 +
#                  (1|submission_id) + (1|itemID), data=select_complement_anaphor, REML=FALSE)
# h2.model = lmer(avg_rt ~ full_or_partial +
#                   (1|submission_id) + (1|itemID), data=select_complement_anaphor, REML=FALSE)
# summary(h2.model)
# anova(h2.null, h2.model)  # It shows that the full-knowledge relates to shorter RT which does not support this H1
# coef(h2.model) 
# # Plots for checking violation of linear assumptions
# plot(fitted(h2.model), residuals(h2.model)) # This one is a bit weird
# hist(residuals(h2.model)) # Left-skewed and not a bell-shape
# qqnorm(residuals(h2.model)) # Looks okay
# 
# 
# 
# # H3: full-knowledge show no significant different effect over the "focus particle" than partial-knowledge in focused trigger sentence 
# select_focused_particle <- df %>%
#   filter(setting == "focused", region == "f_particle")
# head(select_complement_anaphor)
# 
# boxplot(avg_rt ~ full_or_partial,
#         col=c("lightblue"),select_focused_particle) # The differences between two means are very similar
# 
# h3.null = lmer(avg_rt ~ 1 +
#                  (1|submission_id) + (1|itemID), data=select_focused_particle, REML=FALSE)
# h3.model = lmer(avg_rt ~ full_or_partial +
#                   (1|submission_id) + (1|itemID), data=select_focused_particle, REML=FALSE)
# summary(h3.model)
# anova(h3.null, h3.model)  # Differences are small but the p-value is not significant
# coef(h3.model) 
# # Plots for checking violation of linear assumptions
# plot(fitted(h3.model), residuals(h3.model)) # This one is a bit weird
# hist(residuals(h3.model)) # Looks not that normal-distributed
# qqnorm(residuals(h3.model)) # Looks okay
# 
# 
# 
# # H4: No matter full-knowledge or partial knowledge, for scalar trigger, 
# # RTs over all the regions are similar in complement sentences and cancellation sentences
# select_continuation <- df %>%
#   filter(type == "continuation", scalar_in_item_or_not == "scalar")
# head(select_continuation)
# 
# boxplot(avg_rt ~ full_or_partial,
#         col=c("lightblue"),select_continuation) # The differences between two means are very similar
# 
# h4.null = lmer(avg_rt ~ 1 +
#                  (1|submission_id) + (1|itemID), data=select_continuation, REML=FALSE)
# h4.model = lmer(avg_rt ~ full_or_partial +
#                   (1|submission_id) + (1|itemID), data=select_continuation, REML=FALSE)
# summary(h4.model)
# anova(h4.null, h4.model)  # The p-value is still not significant but much better than earlier ones
# coef(h4.model) 
# # Plots for checking violation of linear assumptions
# plot(fitted(h4.model), residuals(h4.model)) # No patterns
# hist(residuals(h4.model)) # A bit left-skewed
# qqnorm(residuals(h4.model)) # Looks okay
# 
# 
# 
# 
# # Others
# # Check the RTs of predicate in Complement Sentences
# select_cmplement_predicate <- df %>%
#   filter(setting == "complement", region == "predicate")
# head(select_cmplement_predicate)
# 
# boxplot(avg_rt ~ full_or_partial*scalar_in_item_or_not,
#         col=c("white","lightblue"),select_cmplement_predicate)
# # The picture seems to predict that: 
# #for "scalar trigger", knowledge doesn't matter;
# #but for "focused trigger", full-knowledge increased the RTs than partial-knowledge
# #which is slightly different than H3
# # H3 says that for "focused particle" part that the knowledge background should not change anything
# df.model = lmer(avg_rt ~ full_or_partial + scalar_in_item_or_not +
#                   (1|submission_id) + (1|itemID), data=select_cmplement_predicate, REML=FALSE)
# summary(df.model)
# df.model.interaction = lmer(avg_rt ~ full_or_partial*scalar_in_item_or_not +
#                 (1|submission_id) + (1|itemID), data=select_cmplement_predicate, REML=FALSE)
# summary(df.model.interaction) # The Chisq value is 0.0136
# 
# anova(df.model, df.model.interaction)
# coef(df.model.interaction) 
# # Plots for checking violation of linear assumptions
# plot(fitted(df.model.interaction), residuals(df.model.interaction)) # No pattern
# hist(residuals(df.model.interaction)) # Not a bell-shape
# qqnorm(residuals(df.model.interaction)) # Looks wired



