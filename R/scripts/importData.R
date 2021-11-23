# import data files
# set working directory to project repository head
setwd("~/honors")

# nzavs
dNZAVSwave11 <- as.data.frame(foreign::read.spss("data/nzavs/NZAVS T11 (2019) SPSS Base Dataset.sav"))
dNZAVSwave9  <- as.data.frame(foreign::read.spss("data/nzavs/NZAVS T09 (2017) SPSS Base Dataset.sav"))
dNZAVSwave6  <- as.data.frame(foreign::read.spss("data/nzavs/NZAVS T06 (2014) SPSS Base Dataset.sav"))
dNZAVSgames  <- as.data.frame(foreign::read.spss("data/nzavs/NZAVS T11 (2019) SPSS Economic Games.sav"))

library(dplyr)

# Subset to only columns/variables that we want to look at
dNZAVSwave11_subset <- select(
  dNZAVSwave11,
  Questionnaire.Num,
  Pers.A_IPIP01.T11,
  Pers.A_IPIP02r.T11,
  Pers.A_IPIP03.T11,
  Pers.A_IPIP04r.T11,
  Env.ClimateChgReal.T11,
  Env.ClimateChgCause.T11,
  Env.ClimateChgConcern.T11,
  Gender.T11,
  Age.T11,
  EthnicCats.T11, 
  E23114T11, 
  E24313T11
)


dNZAVSwave9_subset <- select(
  dNZAVSwave9,
  Questionnaire.Num,
  Env.Eff01.T09,
  Env.Eff02.T09,
  Env.SacMade.T09,
  Env.SacWilling.T09,
  Env.SacNorms.T09,
  Pol.Orient.T09
)

dNZAVSwave6_subset <- select(
  dNZAVSwave6,
  Questionnaire.Num,
  Comp.World01.T06,
  Comp.World02r.T06,
  Env.CarbonRegs.T06
)

dNZAVSgames_subset <- select(
  dNZAVSgames,
  Questionnaire.Num,
  egame.DG.T10,
  egame.TG1.T10,
  egame.TG2.T10,
  egame.payTG.T10
)



# match these datasets by Questionnaire.Num - will only work when games is here
dNZAVS_merged <- inner_join(dNZAVSwave11_subset, dNZAVSgames, dNZAVSwave6_subset, dNZAVSwave9_subset, by = "Questionnaire.Num")

write.csv(dNZAVS_merged, file = "merged_dataset.csv")


# Recoding the -9999 to missing values (NA)
is.na(dNZAVSwave11_subset$Pers.A_IPIP01.T11) <- dNZAVSwave11_subset$Pers.A_IPIP01.T11 == -9999
is.na(dNZAVSwave11_subset$Pers.A_IPIP02r.T11) <- dNZAVSwave11_subset$Pers.A_IPIP02r.T11 == -9999
is.na(dNZAVSwave11_subset$Pers.A_IPIP03.T11) <- dNZAVSwave11_subset$Pers.A_IPIP03.T11 == -9999
is.na(dNZAVSwave11_subset$Pers.A_IPIP04r.T11) <- dNZAVSwave11_subset$Pers.A_IPIP04r.T11 == -9999
is.na(dNZAVSwave11_subset$Env.ClimatechgReal.T11) <- dNZAVSwave11_subset$Env.ClimatechgReal.T11 == -9999
is.na(dNZAVSwave11_subset$Env.ClimatechgCause.T11) <- dNZAVSwave11_subset$Env.ClimatechgCause.T11 == -9999
is.na(dNZAVSwave11_subset$Env.ClimatechgConcern.T11) <- dNZAVSwave11_subset$Env.ClimatechgConcern.T11 == -9999

#Making histograms for everydata point
hist(dNZAVSwave11_subset$Pers.A_IPIP01.T11)
hist(dNZAVSwave11_subset$Pers.A_IPIP02r.T11)
hist(dNZAVSwave11_subset$Pers.A_IPIP03.T11)
hist(dNZAVSwave11_subset$Pers.A_IPIP04r.T11)
hist(dNZAVSwave11_subset$Env.ClimatechgReal.T11)
hist(dNZAVSwave11_subset$Env.ClimatechgCause.T11)
hist(dNZAVSwave11_subset$Env.ClimatechgConcern.T11)

is.na(dNZAVSwave6_subset$Comp.World01.T06) <- dNZAVSwave6_subset$Comp.World01.T06 == -9999
is.na(dNZAVSwave6_subset$Comp.World02r.T06) <- dNZAVSwave6_subset$Comp.World02r.T06 == -9999
is.na(dNZAVSwave6_subset$Env.CarbonRegs.T06) <- dNZAVSwave6_subset$Env.CarbonRegs.T06 == -9999

hist(dNZAVSwave6_subset$Comp.World01.T06)
hist(dNZAVSwave6_subset$Comp.World02r.T06)
hist(dNZAVSwave6_subset$Env.CarbonRegs.T06)       

is.na(dNZAVSwave9_subset$Env.Eff01.T09) <- dNZAVSwave9_subset$Env.Eff01.T09 == -9999
is.na(dNZAVSwave9_subset$Env.Eff02.T09) <- dNZAVSwave9_subset$Env.Eff02.T09 == -9999
is.na(dNZAVSwave9_subset$Env.SacMade.T09) <- dNZAVSwave9_subset$Env.SacMade.T09 == -9999
is.na(dNZAVSwave9_subset$Env.SacWilling.T09) <- dNZAVSwave9_subset$Env.SacWilling.T09 == -9999
is.na(dNZAVSwave9_subset$Env.SacNorms.T09) <- dNZAVSwave9_subset$Env.SacNorms.T09 == -9999


hist(dNZAVSwave9_subset$Env.Eff01.T09)
hist(dNZAVSwave9_subset$Env.Eff02.T09)
hist(dNZAVSwave9_subset$Env.SacMade.T09)
hist(dNZAVSwave9_subset$Env.SacWilling.T09)
hist(dNZAVSwave9_subset$Env.SacNorms.T09)

#Need to do this for GAMES 

# plot(dNZAVSwave11_subset$Pers.A_IPIP01.T11, dNZAVSwave11_subset$Pers.A_IPIP02r.T11)

ids_in_all <- intersect(intersect(intersect(
  dNZAVSwave6_subset$Questionnaire.Num,
  dNZAVSwave9_subset$Questionnaire.Num),
  dNZAVSwave11_subset$Questionnaire.Num
), dNZAVSgames$Questionnaire.Num)

length(ids_in_all)

wave11_demographics.df <- subset(dNZAVSwave11_subset, Questionnaire.Num %in% ids_in_all)

range(wave11_demographics.df$Age.T11)
hist(wave11_demographics.df$Age.T11)
table(cut(wave11_demographics.df$Age.T11, breaks = c(20, 30, 40, 50, 60, 70, 80)))

table(wave11_demographics.df$EthnicCats.T11)

table(wave11_demographics.df$E23114T11)
table(wave11_demographics.df$E24313T11)

table(wave11_demographics.df$Gender.T11)
