library(dplyr)
library(ggplot2)

# read data
df = read.csv("https://raw.githubusercontent.com/whalekeykeeper/mixed_effect_model_for_self_paced_reading_experiment/main/data_processed.csv")
head(df)


# Firgure 1
scalar_trigger_sentences <- df %>%
  filter(setting == "scalar") %>%
  group_by(speakerKnowledge, region)  %>% 
  summarize(m = mean(avg_rt)) 
head(scalar_trigger_sentences)
# Plot
scalar_trigger_sentences %>%
  ggplot( aes(x=region, y=m, group=speakerKnowledge, color=speakerKnowledge)) +
  geom_line(aes(linetype=speakerKnowledge)) + geom_point()+
  scale_y_continuous(breaks = pretty(scalar_trigger_sentences$m, n = 10)) +
  ggtitle("scalar trigger sentences") +
  xlab("region") + ylab("Reading time (ms per word) ")


# Firgure 2
focused_trigger_sentences <- df %>%
  filter(setting == "focused") %>%
  group_by(speakerKnowledge, region)  %>% 
  summarize(m = mean(avg_rt)) 
head(focused_trigger_sentences)
# Plot
focused_trigger_sentences %>%
  ggplot( aes(x=region, y=m, group=speakerKnowledge, color=speakerKnowledge)) +
  geom_line(aes(linetype=speakerKnowledge)) + geom_point()+
  scale_y_continuous(breaks = pretty(focused_trigger_sentences$m, n = 10)) +
  ggtitle("focused trigger sentences")+
  xlab("region") + ylab("Reading time (ms per word) ")


# Firgure 3
complement_with_scalar_trigger <- df %>%
  filter(setting == "complement", triggerType == "scalar") %>%
  group_by(speakerKnowledge, region)  %>% 
  summarize(m = mean(avg_rt)) 
head(complement_with_scalar_trigger)
# Plot
complement_with_scalar_trigger %>%
  ggplot( aes(x=region, y=m, group=speakerKnowledge, color=speakerKnowledge)) +
  geom_line(aes(linetype=speakerKnowledge)) + geom_point()+
  scale_y_continuous(breaks = pretty(complement_with_scalar_trigger$m, n = 10)) +
  ggtitle("complement sentences with scalar trigger") +
  xlab("region") + ylab("Reading time (ms per word) ")


# Firgure 4
complement_with_focused_trigger <- df %>%
  filter(setting == "complement", triggerType == "focused") %>%
  group_by(speakerKnowledge, region)  %>% 
  summarize(m = mean(avg_rt)) 
head(complement_with_focused_trigger)
# Plot
complement_with_focused_trigger %>%
  ggplot( aes(x=region, y=m, group=speakerKnowledge, color=speakerKnowledge)) +
  geom_line(aes(linetype=speakerKnowledge)) + geom_point() +
  scale_y_continuous(breaks = pretty(complement_with_focused_trigger$m, n = 10)) +
  ggtitle("complement sentences with focused trigger") +
  xlab("region") + ylab("Reading time (ms per word) ")


# Firgure 5
cancelation <- df %>%
  filter(setting == "cancelation") %>%
  group_by(speakerKnowledge, region)  %>% 
  summarize(m = mean(avg_rt)) 
head(cancelation)
# Plot
cancelation %>%
  ggplot( aes(x=region, y=m, group=speakerKnowledge, color=speakerKnowledge)) +
  geom_line(aes(linetype=speakerKnowledge)) + geom_point() +
  scale_y_continuous(breaks = pretty(cancelation$m, n = 10)) +
  ggtitle("cancellation sentences") +
  xlab("region") + ylab("Reading time (ms per word) ")

