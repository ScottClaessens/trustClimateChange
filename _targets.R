library(targets)
library(tarchetypes)
library(tidyverse)
source("R/functionsStudy1.R")
source("R/functionsStudy2.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("brms", "cowplot", "grid", "gridExtra", "haven", "mice", 
                            "ordinal", "readxl", "skimr", "tidybayes", "tidyverse"))
# study 1 model combinations
values <- expand_grid(
  outcome = c("Env.Eff01.T09", "Env.Eff02.T09", "Env.ClimateChgReal.T11",
              "Env.ClimateChgCause.T11", "Env.ClimateChgConcern.T11",
              "Env.SacMade.T09", "Env.SacWilling.T09", "Env.SacNorms.T09"),
  predictor = c("egame.TG1.T11 + egame.cmpTG.T11", "compWorld"),
  control = c("", " + Gender.T11", " + Age.T11", " + EthnicCats.T11",
              " + NZREG.T11", " + agree", " + coop", " + Pol.Orient.T09")
)
study1Targets <- tar_map(
  values = values,
  names = "outcome",
  tar_target(m1, fitModel1(dM1, outcome, predictor, control))
)
# pipeline
list(
  
  ###########
  # Study 1 #
  ###########
  
  # load data
  tar_target(fullData, getFullData()),
  tar_target(d1, loadData1()),
  # demographics
  tar_target(demsSample, sumDems(d1)),
  tar_target(demsFull, sumDems(fullData)),
  # multiply impute missing data
  tar_target(dM1, imputeData1(d1)),
  tar_target(plotImpute1, plotImpModel(dM1)),
  # fit models
  study1Targets,
  # plotting
  tar_target(plotTrust, plotTrust1(d1)),
  tar_target(plotCompWorld, plotCompWorld1(d1)),
  # report
  tar_render(report1, "report1.Rmd"),
  
  ###########
  # Study 2 #
  ###########
  
  # load data
  tar_target(d2, loadData2()),
  tar_target(geoProximity,  loadProximityMatrix(d2, "data/study2/countryMatrices/1D Population Proximity.csv")),
  tar_target(lingProximity, loadProximityMatrix(d2, "data/study2/countryMatrices/2D Country Proximity 1pml adj.csv")),
  tar_target(reliProximity, loadProximityMatrix(d2, "data/study2/countryMatrices/3U New3 Country Proximity Anim Eff.csv")),
  # subsetted data
  tar_target(d3, drop_na(d2, wvsPropTrust)),
  tar_target(d4, drop_na(d2, trust)),
  # fit models to c02 data
  # initial models
  tar_target(m2a, fitModel2(d2, geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(Per.capita.CO2.emissions ~ 1 + 
                                 (1 | gr(iso2a, cov = geo)) + 
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli)) +
                                 (1 | iso2)))),
  tar_target(m2b, fitModel2(d2, geoProximity, lingProximity, reliProximity, predictors = TRUE,
                            bf(Per.capita.CO2.emissions ~ 1 + Year +
                                 (1 + Year | gr(iso2a, cov = geo)) + 
                                 (1 + Year | gr(iso2b, cov = ling)) +
                                 (1 + Year | gr(iso2c, cov = reli)) +
                                 (1 + Year | iso2)))),
  tar_target(m2c, fitModel2(d2, geoProximity, lingProximity, reliProximity, predictors = TRUE,
                            bf(Per.capita.CO2.emissions ~ 1 + Year + I(Year^2) +
                                 (1 + Year + I(Year^2) | gr(iso2a, cov = geo)) + 
                                 (1 + Year + I(Year^2) | gr(iso2b, cov = ling)) +
                                 (1 + Year + I(Year^2) | gr(iso2c, cov = reli)) +
                                 (1 + Year + I(Year^2) | iso2)))),
  # world values survey trust models
  tar_target(m3a, fitModel2(d3, geoProximity, lingProximity, reliProximity, predictors = TRUE,
                            bf(Per.capita.CO2.emissions ~ 1 + Year + I(Year^2) +
                                 (1 + Year + I(Year^2) | gr(iso2a, cov = geo)) + 
                                 (1 + Year + I(Year^2) | gr(iso2b, cov = ling)) +
                                 (1 + Year + I(Year^2) | gr(iso2c, cov = reli)) +
                                 (1 + Year + I(Year^2) | iso2)))),
  tar_target(m3b, fitModel2(d3, geoProximity, lingProximity, reliProximity, predictors = TRUE,
                            bf(Per.capita.CO2.emissions ~ 1 + Year + I(Year^2) + 
                                 wvsPropTrust + Year:wvsPropTrust + I(Year^2):wvsPropTrust + 
                                 (1 + Year + I(Year^2) | gr(iso2a, cov = geo)) + 
                                 (1 + Year + I(Year^2) | gr(iso2b, cov = ling)) +
                                 (1 + Year + I(Year^2) | gr(iso2c, cov = reli)) +
                                 (1 + Year + I(Year^2) | iso2)))),
  tar_target(m3c, fitModel2(d3, geoProximity, lingProximity, reliProximity, predictors = TRUE,
                            bf(Per.capita.CO2.emissions ~ 1 + Year + I(Year^2) + 
                                 hdi + Year:hdi + I(Year^2):hdi + 
                                 (1 + Year + I(Year^2) | gr(iso2a, cov = geo)) + 
                                 (1 + Year + I(Year^2) | gr(iso2b, cov = ling)) +
                                 (1 + Year + I(Year^2) | gr(iso2c, cov = reli)) +
                                 (1 + Year + I(Year^2) | iso2)))),
  tar_target(m3d, fitModel2(d3, geoProximity, lingProximity, reliProximity, predictors = TRUE,
                            bf(Per.capita.CO2.emissions ~ 1 + Year + I(Year^2) + 
                                 wvsPropTrust + Year:wvsPropTrust + I(Year^2):wvsPropTrust + 
                                 hdi + Year:hdi + I(Year^2):hdi + 
                                 (1 + Year + I(Year^2) | gr(iso2a, cov = geo)) + 
                                 (1 + Year + I(Year^2) | gr(iso2b, cov = ling)) +
                                 (1 + Year + I(Year^2) | gr(iso2c, cov = reli)) +
                                 (1 + Year + I(Year^2) | iso2)))),
  # global preferences survey trust models
  tar_target(m4a, fitModel2(d4, geoProximity, lingProximity, reliProximity, predictors = TRUE,
                            bf(Per.capita.CO2.emissions ~ 1 + Year + I(Year^2) +
                                 (1 + Year + I(Year^2) | gr(iso2a, cov = geo)) + 
                                 (1 + Year + I(Year^2) | gr(iso2b, cov = ling)) +
                                 (1 + Year + I(Year^2) | gr(iso2c, cov = reli)) +
                                 (1 + Year + I(Year^2) | iso2)))),
  tar_target(m4b, fitModel2(d4, geoProximity, lingProximity, reliProximity, predictors = TRUE,
                            bf(Per.capita.CO2.emissions ~ 1 + Year + I(Year^2) + 
                                 trust + Year:trust + I(Year^2):trust + 
                                 (1 + Year + I(Year^2) | gr(iso2a, cov = geo)) + 
                                 (1 + Year + I(Year^2) | gr(iso2b, cov = ling)) +
                                 (1 + Year + I(Year^2) | gr(iso2c, cov = reli)) +
                                 (1 + Year + I(Year^2) | iso2)))),
  tar_target(m4c, fitModel2(d4, geoProximity, lingProximity, reliProximity, predictors = TRUE,
                            bf(Per.capita.CO2.emissions ~ 1 + Year + I(Year^2) + 
                                 hdi + Year:hdi + I(Year^2):hdi + 
                                 (1 + Year + I(Year^2) | gr(iso2a, cov = geo)) + 
                                 (1 + Year + I(Year^2) | gr(iso2b, cov = ling)) +
                                 (1 + Year + I(Year^2) | gr(iso2c, cov = reli)) +
                                 (1 + Year + I(Year^2) | iso2)))),
  tar_target(m4d, fitModel2(d4, geoProximity, lingProximity, reliProximity, predictors = TRUE,
                            bf(Per.capita.CO2.emissions ~ 1 + Year + I(Year^2) + 
                                 trust + Year:trust + I(Year^2):trust + 
                                 hdi + Year:hdi + I(Year^2):hdi + 
                                 (1 + Year + I(Year^2) | gr(iso2a, cov = geo)) + 
                                 (1 + Year + I(Year^2) | gr(iso2b, cov = ling)) +
                                 (1 + Year + I(Year^2) | gr(iso2c, cov = reli)) +
                                 (1 + Year + I(Year^2) | iso2)))),
  # model comparison
  tar_target(looComp2, loo_compare(m2a, m2b, m2c)),
  tar_target(looComp3, loo_compare(m3a, m3b, m3c, m3d)),
  tar_target(looComp4, loo_compare(m4a, m4b, m4c, m4d)),
  # plotting
  tar_target(plotPredCountry,      plotPredCountry2(m2c)),
  tar_target(plotPredTrust,        plotPredTrust2(m3b, m4b, file = "figures/plotTrustC02.pdf")),
  tar_target(plotPredTrustWithHDI, plotPredTrust2(m3d, m4d, file = "figures/plotTrustC02WithHDI.pdf")),
  tar_target(plotPredHDI,          plotPredHDI2(m3c, m4c, file = "figures/plotHDIC02.pdf")),
  tar_target(plotPredHDIWithTrust, plotPredHDI2(m3d, m4d, file = "figures/plotHDIC02WithTrust.pdf")),
  
  # load sufficiency data
  tar_target(d5, loadData3()),
  # fit models to sufficiency data
  # wvs trust models
  tar_target(m5a, fitModel3(drop_na(d5, sufficiency, wvsPropTrust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(sufficiency ~ 1 + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m5b, fitModel3(drop_na(d5, sufficiency, wvsPropTrust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(sufficiency ~ 1 + wvsPropTrust + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m5c, fitModel3(drop_na(d5, sufficiency, wvsPropTrust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(sufficiency ~ 1 + hdi + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m5d, fitModel3(drop_na(d5, sufficiency, wvsPropTrust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(sufficiency ~ 1 + wvsPropTrust + hdi +
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  # gps trust models
  tar_target(m6a, fitModel3(drop_na(d5, sufficiency, trust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(sufficiency ~ 1 + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m6b, fitModel3(drop_na(d5, sufficiency, trust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(sufficiency ~ 1 + trust + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m6c, fitModel3(drop_na(d5, sufficiency, trust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(sufficiency ~ 1 + hdi + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m6d, fitModel3(drop_na(d5, sufficiency, trust),
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(sufficiency ~ 1 + trust + hdi +
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  # model comparison
  tar_target(looComp5, loo_compare(m5a, m5b, m5c, m5d)),
  tar_target(looComp6, loo_compare(m6a, m6b, m6c, m6d)),
  # plotting
  tar_target(plotSufficiency, plotPredSufficiency2(m5b, m5d, m6b, m6d)),
  
  # load pew data
  tar_target(d6, loadData4()),
  # fit models to pew data
  # wvs trust models
  tar_target(m7a, fitModel4(drop_na(d6, propMajorThreat, wvsPropTrust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(propMajorThreat ~ 1 + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m7b, fitModel4(drop_na(d6, propMajorThreat, wvsPropTrust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(propMajorThreat ~ 1 + wvsPropTrust + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m7c, fitModel4(drop_na(d6, propMajorThreat, wvsPropTrust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(propMajorThreat ~ 1 + hdi + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m7d, fitModel4(drop_na(d6, propMajorThreat, wvsPropTrust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(propMajorThreat ~ 1 + wvsPropTrust + hdi +
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  # gps trust models
  tar_target(m8a, fitModel4(drop_na(d6, propMajorThreat, trust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(propMajorThreat ~ 1 + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m8b, fitModel4(drop_na(d6, propMajorThreat, trust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(propMajorThreat ~ 1 + trust + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m8c, fitModel4(drop_na(d6, propMajorThreat, trust), 
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(propMajorThreat ~ 1 + hdi + 
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  tar_target(m8d, fitModel4(drop_na(d6, propMajorThreat, trust),
                            geoProximity, lingProximity, reliProximity, predictors = FALSE,
                            bf(propMajorThreat ~ 1 + trust + hdi +
                                 (1 | gr(iso2a, cov = geo)) +
                                 (1 | gr(iso2b, cov = ling)) +
                                 (1 | gr(iso2c, cov = reli))))),
  # model comparison
  tar_target(looComp7, loo_compare(m7a, m7b, m7c, m7d)),
  tar_target(looComp8, loo_compare(m8a, m8b, m8c, m8d)),
  # plotting
  tar_target(plotPew, plotPredPew2(m7b, m7d, m8b, m8d))
)