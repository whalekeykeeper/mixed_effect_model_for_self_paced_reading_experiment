library(dplyr)
library(lme4)
library(performance)

# read data
df = read.csv("https://raw.githubusercontent.com/whalekeykeeper/mixed_effect_model_for_self_paced_reading_experiment/main/data_processed.csv")

# Log-transform data
df$avg_rt=log(df$avg_rt)
head(df)

# Create a simple linear regression model and use residual plots to check assumptions for a linear regression model
xmdl= lm(avg_rt ~ speakerKnowledge, df)
summary(xmdl)
plot(fitted(xmdl), residuals(xmdl)) # This one looks a bit weird
hist(residuals(xmdl)) # Left-skewed
qqnorm(residuals(xmdl)) # Looks okay
dfbeta(xmdl)


# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Trigger Sentences
# Full data
# Quantifier: S/some of
data <- df %>%
  filter(type == "trigger", region == "1_s_quantifier" | region == "1_f_quantifier")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.1 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                  (1|submission_id), data=data, REML=FALSE)
# Items
model.2 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                               (1|itemID), data=data, REML=FALSE)
summary(model.1)
summary(model.2)
# anova(model.1, model.2)
coef(model.1)
coef(model.2)

# Plots
plot(fitted(model.1), residuals(model.1))
hist(residuals(model.1)) 
qqnorm(residuals(model.1)) 


# Trigger Sentences
# Full data
# Region 2
data <- df %>%
  filter(type == "trigger", region == "3_f_ntw_1" | region == "2_s_ntw_1")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.3 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge*triggerType |submission_id), data=data, REML=FALSE)
# Items
model.4 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + triggerType |itemID), data=data, REML=FALSE)
summary(model.3)
summary(model.4)
coef(model.3)
coef(model.4)



# Trigger Sentences
# Full data
# Region 3
data <- df %>%
  filter(type == "trigger", region == "4_f_ntw_2" | region == "3_s_ntw_2")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.5 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.6 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.5)
summary(model.6)
coef(model.5)
coef(model.6)
r2_nakagawa(model.5, by_group = FALSE, tolerance = 1e-05)
r2_nakagawa(model.6, by_group = FALSE, tolerance = 1e-05)


# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Trigger Sentences
# Scalar trigger data
# Quantifier
data <- df %>%
  filter(setting == "scalar", region == "1_s_quantifier")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.7 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.8 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.7)
summary(model.8)
coef(model.7)
coef(model.8)


# Trigger Sentences
# Scalar trigger data
# Region 2
data <- df %>%
  filter(setting == "scalar", region == "2_s_ntw_1")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.9 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.10 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.9)
summary(model.10)
coef(model.9)
coef(model.10)



# Trigger Sentences
# Scalar trigger data
# Region 3
data <- df %>%
  filter(setting == "scalar", region == "3_s_ntw_2")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.11 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.12 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.11)
summary(model.12)
# anova(model.1, model.2)
coef(model.11)
coef(model.12)


# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Trigger Sentences
# Focused trigger data
# Focus: Only
data <- df %>%
  filter(setting == "focused", region == "1_f_particle")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.13 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.14 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.13)
summary(model.14)
coef(model.13)
coef(model.14)



# Trigger Sentences
# Focused trigger data
# Quantifier
data <- df %>%
  filter(setting == "focused", region == "2_f_quantifier")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.15 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.16 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.15)
summary(model.16)
coef(model.15)
coef(model.16)



# Trigger Sentences
# Focused trigger data
# Region 2
data <- df %>%
  filter(setting == "focused", region == "3_f_ntw_1")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.17 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge|submission_id), data=data, REML=FALSE)
# Items
model.18 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge|itemID), data=data, REML=FALSE)
summary(model.17)
summary(model.18)
coef(model.17)
coef(model.18)




# Trigger Sentences
# Focused trigger data
# Region 3
data <- df %>%
  filter(setting == "focused", region == "4_f_ntw_2")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.19 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.20 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.19)
summary(model.20)
coef(model.19)
coef(model.20)




# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Cancelation Sentences
#
data <- df %>%
  filter(setting == "complement", region == "1_anaphor")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.21 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.22 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.21)
summary(model.22)
coef(model.21)
coef(model.22)
r2_nakagawa(model.21, by_group = FALSE, tolerance = 1e-05)
r2_nakagawa(model.22, by_group = FALSE, tolerance = 1e-05)



# Complement Sentences
# Full data
# Predicate
data <- df %>%
  filter(setting == "complement", region == "2_predicate")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.23 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge*triggerType|submission_id), data=data, REML=FALSE)
# Items
model.24 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge + triggerType|itemID), data=data, REML=FALSE)
summary(model.23)
summary(model.24)
coef(model.23)
coef(model.24)
r2_nakagawa(model.24, by_group = FALSE, tolerance = 1e-05)



# Complement Sentences
# Full data
# Clause break
data <- df %>%
  filter(setting == "complement", region == "3_clause_boundary")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.25 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.26 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.25)
summary(model.26)
coef(model.25)
coef(model.26)
r2_nakagawa(model.25, by_group = FALSE, tolerance = 1e-05)
r2_nakagawa(model.26, by_group = FALSE, tolerance = 1e-05)




# Complement Sentences
# Full data
# Next two words
data <- df %>%
  filter(setting == "complement", region == "4_next_two_words")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.27 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.28 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.27)
summary(model.28)
# anova(model.1, model.2)
coef(model.27)
coef(model.28)
r2_nakagawa(model.27, by_group = FALSE, tolerance = 1e-05)
r2_nakagawa(model.28, by_group = FALSE, tolerance = 1e-05)



# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Complement Sentences
# Scalar trigger data
# Anaphor: The rest
data <- df %>%
  filter(setting == "complement" & triggerType == "scalar", region == "1_anaphor")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.29 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.30 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.29)
summary(model.30)
coef(model.29)
coef(model.30)


# Complement Sentences
# Scalar trigger data
# Predicate
data <- df %>%
  filter(setting == "complement" & triggerType == "scalar", region == "2_predicate")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.31 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge|submission_id), data=data, REML=FALSE)
# Items
model.32 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.31)
summary(model.32)
coef(model.31)
coef(model.32)


# Complement Sentences
# Scalar trigger data
# Clause break
data <- df %>%
  filter(setting == "complement" & triggerType == "scalar", region == "3_clause_boundary")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.33 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge|submission_id), data=data, REML=FALSE)
# Items
model.34 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.33)
summary(model.34)
coef(model.33)
coef(model.34)


# Complement Sentences
# Scalar trigger data
# Next two words
data <- df %>%
  filter(setting == "complement" & triggerType == "scalar", region == "4_next_two_words")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.35 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.36 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.35)
summary(model.36)
coef(model.35)
coef(model.36)


# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Complement Sentences
# Focused trigger data
# Anaphor: The rest
data <- df %>%
  filter(setting == "complement" & triggerType == "focused", region == "1_anaphor")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.37 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.38 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.37)
summary(model.38)
coef(model.37)
coef(model.38)



# Complement Sentences
# Scalar trigger data
# Predicate
data <- df %>%
  filter(setting == "complement" & triggerType == "focused", region == "2_predicate")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.39 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge|submission_id), data=data, REML=FALSE)
# Items
model.40 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.39)
summary(model.40)
coef(model.39)
coef(model.40)


# Complement Sentences
# Scalar trigger data
# Clause break
data <- df %>%
  filter(setting == "complement" & triggerType == "focused", region == "3_clause_boundary")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.41 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge|submission_id), data=data, REML=FALSE)
# Items
model.42 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.41)
summary(model.42)
coef(model.41)
coef(model.42)




# Complement Sentences
# Scalar trigger data
# Next two words
data <- df %>%
  filter(setting == "complement" & triggerType == "focused", region == "4_next_two_words")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.43 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.44 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.43)
summary(model.44)
coef(model.43)
coef(model.44)




# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
# Cancelation Sentences
# Scalar trigger data
# Region 1
data <- df %>%
  filter(setting == "cancelation", region == "cancelation_1")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.45 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.46 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.45)
summary(model.46)
coef(model.45)
coef(model.46)


# Cancelation Sentences
# Scalar trigger data
# Region 2
data <- df %>%
  filter(setting == "cancelation", region == "cancelation_2")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.47 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge|submission_id), data=data, REML=FALSE)
# Items
model.48 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.47)
summary(model.48)
coef(model.47)
coef(model.48)


# Cancelation Sentences
# Scalar trigger data
# Region 3
data <- df %>%
  filter(setting == "cancelation", region == "cancelation_3")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.49 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.50 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|itemID), data=data, REML=FALSE)
summary(model.49)
summary(model.50)
coef(model.49)
coef(model.50)


# Cancelation Sentences
# Scalar trigger data
# Region 4
data <- df %>%
  filter(setting == "cancelation", region == "cancelation_4")
head(data)

boxplot(avg_rt ~ speakerKnowledge,
        col=c("lightblue"),data)
boxplot(avg_rt ~ setting,
        col=c("lightblue"),data)

# Participants
model.51 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1|submission_id), data=data, REML=FALSE)
# Items
model.52 = lmer(avg_rt ~ speakerKnowledge*triggerType +
                 (1 + speakerKnowledge|itemID), data=data, REML=FALSE)
summary(model.51)
summary(model.52)
coef(model.51)
coef(model.52)



# 
# 
# # H1: full-knowledge caused longer RT over the scalar quantifier of scalar trigger sentences than partial-knowledge
# select_scalar_quantifier <- df %>%
#   filter(setting == "scalar", is_a_quantifier == "yes")
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
#   filter(setting == "complement", region == "1_anaphor")
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
#   filter(type == "continuation", item_has_a_scalar_trigger_sentence == "yes")
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




