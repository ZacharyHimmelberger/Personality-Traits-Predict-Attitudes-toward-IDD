# Personality and ID
# Data Processing

# Import Packages

library(readxl)
library(tidyverse)

# Load Data

raw.df <- read_excel("data/attitudes_final.xlsx", 
                     sheet=1, 
                     skip=1)
head(raw.df, n=10)

# Reverse Scoring

raw.df[, c('BFI_2.r', 'BFI_6.r', 'BFI_8.r', 'BFI_9.r', 'BFI_12.r', 
           'BFI_18.r', 'BFI_21.r', 'BFI_23.r', 'BFI_24.r', 'BFI_27.r', 
           'BFI_31.r', 'BFI_34.r', 'BFI_35.r', 'BFI_37.r', 'BFI_41.r', 
           'BFI_43.r')] <- 6 - raw.df[, c('BFI_2', 'BFI_6', 'BFI_8', 
                                          'BFI_9', 'BFI_12', 'BFI_18', 
                                          'BFI_21', 'BFI_23', 'BFI_24', 
                                          'BFI_27', 'BFI_31', 'BFI_34', 
                                          'BFI_35', 'BFI_37', 'BFI_41', 
                                          'BFI_43')] 
raw.df[, c('Attitudes_1.r', 'Attitudes_4.r', 'Attitudes_6.r', 
           'Attitudes_7.r', 'Attitudes_9.r', 'Attitudes_10.r', 
           'Attitudes_12.r', 'Attitudes_13.r', 'Attitudes_15.r', 
           'Attitudes_16.r', 'Attitudes_18.r', 'Attitudes_20.r', 
           'Attitudes_21.r', 'Attitudes_22.r', 'Attitudes_23.r', 
           'Attitudes_25.r', 'Attitudes_26.r', 
           'Attitudes_27.r')] <- 10 - raw.df[, 
                                             c('Attitudes_1', 'Attitudes_4', 
                                               'Attitudes_6', 'Attitudes_7', 
                                               'Attitudes_9', 'Attitudes_10', 
                                               'Attitudes_12', 'Attitudes_13', 
                                               'Attitudes_15', 'Attitudes_16', 
                                               'Attitudes_18', 'Attitudes_20', 
                                               'Attitudes_21', 'Attitudes_22', 
                                               'Attitudes_23', 'Attitudes_25', 
                                               'Attitudes_26', 'Attitudes_27')]
raw.df[,c('Quality_7.r','Quality_8.r')] <- 10 - raw.df[,c('Quality_7',
                                                          'Quality_8')]
raw.df[,c('Quantity_4.r')] <- 10 - raw.df[,c('Quantity_4')]
raw.df[,c('Knowledge_9.r','Knowledge_11.r')] <- 10 - raw.df[,c('Knowledge_9',
                                                               'Knowledge_11')]

# Create Composite Variables

raw.df$openness <- raw.df %>%
  select(BFI_5, BFI_10, BFI_15, BFI_20, 
         BFI_25, BFI_30, BFI_35.r, BFI_40, 
         BFI_41.r, BFI_44) %>%
  rowMeans(na.rm=TRUE)

raw.df$conscientiousness <- raw.df %>%
  select(BFI_3, BFI_8.r, BFI_13, BFI_18.r, 
         BFI_23.r, BFI_28, BFI_33, BFI_38, 
         BFI_43.r) %>%
  rowMeans(na.rm=TRUE)

raw.df$extroversion <- raw.df %>%
  select(BFI_1, BFI_6.r, BFI_11, BFI_16, 
         BFI_21.r, BFI_26, BFI_31.r, BFI_36) %>%
  rowMeans(na.rm=TRUE)

raw.df$agreeableness <- raw.df %>%
  select(BFI_2.r, BFI_7, BFI_12.r, BFI_17, 
         BFI_22, BFI_27.r, BFI_32, BFI_37.r, 
         BFI_42) %>%
  rowMeans(na.rm=TRUE)

raw.df$neuroticism <- raw.df %>%
  select(BFI_4, BFI_9.r, BFI_14, BFI_19, 
         BFI_24.r, BFI_29, BFI_34.r, BFI_39) %>%
  rowMeans(na.rm=TRUE)

raw.df$attitude <- raw.df %>%
  select(Attitudes_1.r, Attitudes_2, Attitudes_3,
         Attitudes_4.r, Attitudes_5, Attitudes_6.r,
         Attitudes_7.r, Attitudes_8, Attitudes_9.r,
         Attitudes_10.r, Attitudes_11, Attitudes_12.r,
         Attitudes_13.r, Attitudes_14, Attitudes_15.r,
         Attitudes_16.r, Attitudes_17, Attitudes_18.r,
         Attitudes_19, Attitudes_20.r, Attitudes_21.r,
         Attitudes_22.r, Attitudes_23.r, Attitudes_24,
         Attitudes_25.r, Attitudes_26.r, Attitudes_27.r,
         Attitudes_28, Attitudes_29) %>%
  rowMeans(na.rm=TRUE)

raw.df$integration <- raw.df %>%
  select(Attitudes_1.r, Attitudes_2, Attitudes_7.r, Attitudes_13.r, 
         Attitudes_17, Attitudes_23.r, Attitudes_29) %>%
  rowMeans(na.rm=TRUE)

raw.df$social.distance <- raw.df %>%
  select(Attitudes_3, Attitudes_5, Attitudes_11, Attitudes_15.r,
         Attitudes_18.r, Attitudes_19, Attitudes_24, Attitudes_27.r) %>%
  rowMeans(na.rm=TRUE)

raw.df$private.rights <- raw.df %>%
  select(Attitudes_6.r, Attitudes_8, Attitudes_12.r, Attitudes_14,
         Attitudes_20.r, Attitudes_22.r, Attitudes_28) %>%
  rowMeans(na.rm=TRUE)

raw.df$subtle.derogatory.beliefs <- raw.df %>%
  select(Attitudes_4.r, Attitudes_9.r, Attitudes_10.r, Attitudes_16.r, 
         Attitudes_21.r, Attitudes_25.r, Attitudes_26.r) %>%
  rowMeans(na.rm=TRUE)

raw.df$quality <- raw.df %>%
  select(Quality_2, Quality_3, Quality_4,
         Quality_5, Quality_6, Quality_7.r, Quality_8.r) %>%
  rowMeans(na.rm=TRUE)

raw.df$quantity <- raw.df %>%
  select(Quantity_1, Quantity_2, Quantity_3, Quantity_4.r,
         Quantity_5, Quantity_6, Quantity_7, Quantity_8,
         Quantity_9, Quantity_10) %>%
  rowMeans(na.rm=TRUE)

raw.df$knowledge <- raw.df %>%
  select(Knowledge_1, Knowledge_2, Knowledge_3, Knowledge_4,
         Knowledge_5, Knowledge_6, Knowledge_7, Knowledge_8,
         Knowledge_9.r, Knowledge_10, Knowledge_11.r, Knowledge_12,
         Knowledge_13, Knowledge_14, Knowledge_15, Knowledge_16) %>%
  rowMeans(na.rm=TRUE)

raw.df$social.sum.true <- apply(
  raw.df[, 
         c('S_Desirability_1', 'S_Desirability_2', 'S_Desirability_4',
           'S_Desirability_7', 'S_Desirability_8', 'S_Desirability_13',
           'S_Desirability_16','S_Desirability_17', 'S_Desirability_18',
           'S_Desirability_20', 'S_Desirability_21', 'S_Desirability_24',
           'S_Desirability_25', 'S_Desirability_26', 'S_Desirability_27',
           'S_Desirability_29', 'S_Desirability_31', 'S_Desirability_33')],
  1, function(x) sum(x == 1))

raw.df$social.sum.false <- apply(
  raw.df[, 
         c('S_Desirability_3', 'S_Desirability_5', 'S_Desirability_6',
           'S_Desirability_9', 'S_Desirability_10', 'S_Desirability_11',
           'S_Desirability_12', 'S_Desirability_14', 'S_Desirability_15',
           'S_Desirability_19', 'S_Desirability_22', 'S_Desirability_23',
           'S_Desirability_28', 'S_Desirability_30', 'S_Desirability_32')],
  1, function(x) sum(x == 2))

raw.df$social.desirability <- raw.df$social.sum.true + raw.df$social.sum.false 

# Recode Variables
raw.df$gender <- recode(raw.df$Q10,
                        "1" = "male",
                        "2" = "female",
                        "3" = "non-binary")

raw.df$ethnicity <-recode(raw.df$Q12,
                          "1" = "black",
                          "2" = "black Hispanic",
                          "3" = "white Hispanic",
                          "4" = "American Indian",
                          "5" = "Asian or Pacific Islander",
                          "6" = "white",
                          "7" = "other",
                          "8" = "prefer not to answer")

# Select Columns and Check for Missing Values
working.df <- raw.df %>%
  select(gender, ethnicity, age=Age, social.desirability,
         openness, conscientiousness, extroversion, agreeableness,
         neuroticism, attitude, integration, social.distance, 
         private.rights, subtle.derogatory.beliefs, quality, quantity,
         knowledge)

working.df %>%
  is.na() %>%
  rowSums() %>% 
  table() 

working.df[rowSums(is.na(working.df)) > 3,]

# Remove Participant with Significant Missing Data

df <- working.df[rowSums(is.na(working.df)) <= 3,]

# Center Quantitative Predictors

temp <- df %>%
  select(social.desirability, openness, conscientiousness, extroversion,
         agreeableness, neuroticism,  quantity, quality,
         knowledge) %>%
  scale(center=TRUE, scale=FALSE)

temp <- as.data.frame(temp, row.names = NULL)
temp <- set_names(temp, paste0(names(temp), ".c"))

# Merge DataFrames, Recode Categorical Predictors, and Drop Missing Data

analysis.df <- df %>%
  mutate(male = recode(gender,
                       "male" = 1,
                       .default = 0)) %>%
  mutate(gender.male = recode(gender,
                              "male" = 1,
                              .default = 0)) %>%
  mutate(gender.non.binary = recode(gender,
                                    "non-binary" = 1,
                                    .default = 0)) %>%
  mutate(non.white = recode(ethnicity,
                            "white" = 0,
                            .default = 1)) %>%
  select(age, gender, ethnicity, attitude, integration, social.distance, private.rights,
         subtle.derogatory.beliefs, male, non.white, gender.male, gender.non.binary,
         social.desirability, openness,
         conscientiousness, extroversion, agreeableness, neuroticism,
         quantity, quality, knowledge) %>%
  bind_cols(temp) %>%
  drop_na(attitude, social.desirability.c, openness.c,
          conscientiousness.c, extroversion.c, agreeableness.c, neuroticism.c,
          quantity.c, quality.c, knowledge.c)
