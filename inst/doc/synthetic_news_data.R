## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----observed_data_generation-------------------------------------------------
library(readr)
library(dplyr)
df <- suppressWarnings(read_csv("https://raw.githubusercontent.com/StatsGary/SyntheticNEWSData/main/observed_news_data.csv") %>% 
  dplyr::select(everything(), -X1))

glimpse(df)


## ----synth--------------------------------------------------------------------
library(synthpop)  
syn_df <- syn(df,seed=4321)
#### synthetic data
synthetic_news_data <- syn_df$syn
glimpse(synthetic_news_data)


## ----visuals------------------------------------------------------------------
library(ggplot2)
#Create temperature tibbles to compare observed vs synthetically generated labels
obs <- tibble(label="observed_data", value = df$temp)
synth <- tibble(label="synthetic_data",value = synthetic_news_data$temp)

#Merge the frames together to get a comparison
merged <- obs %>% 
  bind_rows(synth)

#Create the plot
plot <- merged %>% 
  ggplot(aes(value, fill = label)) +
  geom_histogram(alpha = 0.9, position = 'identity')  + theme_minimal() + 
  scale_fill_manual(values=c("#BCBDC1", "#2061AC")) +
  labs(title="Observed vs Synthetically NEWS values",
       subtitle="Based on NEWS Temperature score",
       x="NEWS Temperature Score", y="Score frequency") + 
  theme(legend.position = "none")

print(plot)


