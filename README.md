#Here's an example R code to predict voter positions in the 2014 EES dataset (assuming you have downloaded ZA5160_v4-1-0.dta from the GESIS website)

library(haven)
EES_2014 <- read_stata("ZA5160_v4-1-0.dta")

install.packages("remotes")  # if not installed yet
remotes::install_github("patrickmellacher/predictVoterIdeologyR")

library(predictVoterIdeologyR)

EES_2014$econ_interven <- EES_2014$qpp17_1
EES_2014[EES_2014$econ_interven < 0, ]$econ_interven <- NA
EES_2014$redistribution <- EES_2014$qpp17_2
EES_2014[EES_2014$redistribution < 0, ]$redistribution <- NA
EES_2014$sociallifestyle <- EES_2014$qpp17_4
EES_2014[EES_2014$sociallifestyle < 0, ]$sociallifestyle <- NA
EES_2014$civlib_laworder <- EES_2014$qpp17_5
EES_2014[EES_2014$civlib_laworder < 0, ]$civlib_laworder <- NA
EES_2014$immigration <- EES_2014$qpp17_6
EES_2014[EES_2014$immigration < 0, ]$immigration <- NA
EES_2014$immigrate_policy <- 10 - EES_2014$immigration #this question is reverse-coded in the EES dataset
EES_2014$environment <- EES_2014$qpp17_8

# df: data frame containing all required variables
EES_2014_incl_prediction <- predict_voter_ideology(df = EES_2014)
