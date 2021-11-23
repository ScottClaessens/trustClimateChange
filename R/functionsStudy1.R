# functions for study 1

# get full dataset
getFullData <- function() {
  read_sav("data/study1/NZAVS T11 (2019) SPSS Base Dataset.sav",
           col_select = c(Gender.T11, Age.T11, EthnicCats.T11, NZREG.T11)) %>%
    mutate(
      Gender.T11       = factor(ifelse(Gender.T11 == 0, "Female", "Male")),
      EthnicCats.T11   = factor(ifelse(EthnicCats.T11 == 1, "Pakeha",
                                       ifelse(EthnicCats.T11 == 2, "Maori",
                                              ifelse(EthnicCats.T11 == 3, "Pacific", "Asian")))),
      NZREG.T11        = as.numeric(NZREG.T11))
}

# load data
loadData1 <- function() {
  # load in datasets
  wave6 <-
    read_sav("data/study1/NZAVS T06 (2014) SPSS Base Dataset.sav", 
             col_select = c(Questionnaire.Num, Comp.World01.T06, Comp.World02r.T06, Env.CarbonRegs.T06))
  wave9 <-
    read_sav("data/study1/NZAVS T09 (2017) SPSS Base Dataset.sav",
             col_select = c(Questionnaire.Num, Env.Eff01.T09, Env.Eff02.T09, Env.SacMade.T09, 
                            Env.SacWilling.T09, Env.SacNorms.T09, Pol.Orient.T09))
  wave11 <-
    read_sav("data/study1/NZAVS T11 (2019) SPSS Base Dataset.sav", 
            col_select = c(Questionnaire.Num, Pers.A_IPIP01.T11, Pers.A_IPIP02r.T11, Pers.A_IPIP03.T11,
                           Pers.A_IPIP04r.T11, Env.ClimateChgReal.T11, Env.ClimateChgCause.T11,
                           Env.ClimateChgConcern.T11, Gender.T11, Age.T11, EthnicCats.T11, NZREG.T11))
  egames <-
    read_sav("data/study1/NZAVS T11 (2019) SPSS Economic Games.sav", 
             col_select = c(Questionnaire.Num, egame.TG1.T11, egame.TG2.T11, egame.DG.T11, egame.PGG.T11,
                            egame.cmpTG.T11, egame.cmpDG.T11, egame.cmpPGG.T11))
  out <-
    # merging
    wave6 %>%
    full_join(wave9, by = "Questionnaire.Num") %>%
    full_join(wave11, by = "Questionnaire.Num") %>%
    full_join(egames, by = "Questionnaire.Num") %>%
    # filtering
    filter(!is.na(egame.TG1.T11)) %>% # keep only participants who completed economic games
    # clean up data columns
    mutate(
      Gender.T11       = factor(ifelse(Gender.T11 == 0, "Female", "Male")),
      EthnicCats.T11   = factor(ifelse(EthnicCats.T11 == 1, "Pakeha",
                                       ifelse(EthnicCats.T11 == 2, "Maori",
                                              ifelse(EthnicCats.T11 == 3, "Pacific", "Asian")))),
      egame.TG1.T11    = factor(ifelse(egame.TG1.T11 == 0, "No transfer", "Transfer")),
      egame.cmpTG.T11  = factor(ifelse(egame.cmpTG.T11 == 1, "Correct", "Incorrect")),
      egame.cmpDG.T11  = factor(ifelse(egame.cmpDG.T11 == 1, "Correct", "Incorrect")),
      egame.cmpPGG.T11 = factor(ifelse(egame.cmpPGG.T11 == 3, "Correct", "Incorrect")),
      NZREG.T11        = as.numeric(NZREG.T11)
    )
  return(out)
}

# summarise demographics
sumDems <- function(data) {
  data %>%
    select(Gender.T11, Age.T11, EthnicCats.T11, NZREG.T11) %>%
    skim()
}

# impute data
imputeData1 <- function(d1) {
  # get predictor matrix
  pred <- mice(d1, maxit = 0)$predictorMatrix
  pred[,"Questionnaire.Num"] <- 0 # remove QNum as predictor
  # run the algorithm
  out <- mice(d1, m = 10, predictorMatrix = pred, seed = 123)
  return(out)
}

# plot imputed data
plotImpModel <- function(dM) {
  # variables to plot
  vars <- select(dM$data, -Questionnaire.Num, -Gender.T11, -EthnicCats.T11, -starts_with("egame.")) %>% colnames()
  # create empty list of plots
  plots <- list()
  # create density plot for each variable
  for (i in 1:length(vars)) plots[[i]] <- densityplot(dM, as.formula(paste("~", vars[i])))
  # put all together in one plot
  out <- cowplot::plot_grid(plotlist = plots, ncol = 4, nrow = 4)
  # save figure to file
  ggsave(out, file = "figures/plotImputationModel.pdf", height = 10, width = 10)
  return(out)
}

# fit model 1
fitModel1 <- function(dM1, outcome, predictor, control) {
  # create composite variables in dM
  dM1 <-
    dM1 %>%
    complete("long", include = TRUE) %>%
    # outcome variables are ordered factors
    mutate_at(vars(starts_with("Env.")), as.ordered) %>%
    mutate(
      # create standardised composites
      compWorld       = as.numeric(scale( ((Comp.World01.T06 + Comp.World02r.T06) / 2) )),
      agree           = as.numeric(scale( ((Pers.A_IPIP01.T11 + Pers.A_IPIP02r.T11 + Pers.A_IPIP03.T11 + Pers.A_IPIP04r.T11) / 4) )),
      coop            = as.numeric(scale( (((egame.TG2.T11 / 150) + (egame.DG.T11 / 100) + (egame.PGG.T11 / 100)) / 3) )),
      # standardise other variables
      Age.T11         = as.numeric(scale(Age.T11)),
      NZREG.T11       = as.numeric(scale(NZREG.T11)),
      Pol.Orient.T09  = as.numeric(scale(Pol.Orient.T09))
      ) %>%
    # revert back to mids format
    as.mids()
  # create model formula
  f <- formula(paste(outcome, " ~ ", predictor, control, collapse = " "))
  # get data list for model fitting
  imp.list <- list()
  for (i in 1:dM1$m) imp.list[[i]] <- complete(dM1, i, include = FALSE)
  # fit the models
  mods <- lapply(imp.list, function(d) clm(f, data = d))
  out <- pool(mods)
  return(out)
}

# plot trust game
plotTrust1 <- function(d1) {
  # generic plotting function
  plotFun <- function(d, dv, ylab) {
    avg <-
      d %>%
      group_by(egame.TG1.T11) %>%
      summarise(mean = mean(!!sym(dv), na.rm = TRUE),
                se = sd(!!sym(dv), na.rm = TRUE) / sqrt(n()))
    d %>%
      drop_na() %>%
      group_by(egame.TG1.T11, !!sym(dv)) %>%
      summarise(Freq = n(), .groups = "drop") %>%
      ggplot(aes(x = egame.TG1.T11)) +
      geom_point(aes(y = !!sym(dv), size = Freq), shape = 1, alpha = 0.2) +
      geom_pointrange(data = avg, aes(y = mean, ymin = mean - (2*se), ymax = mean + (2*se)), size = 0.5, shape = 18) +
      scale_size(range = c(0, 5)) +
      scale_y_continuous(name = ylab, breaks = 1:7) +
      labs(x = NULL) +
      theme_classic() +
      theme(axis.title.y = element_text(size = 10))
  }
  # separate plots
  pA <- plotFun(d1, "Env.Eff01.T09"            , "\nBy taking personal action I believe\nI can make a positive difference\nto environmental problems")
  pB <- plotFun(d1, "Env.Eff02.T09"            , " \n\nI feel I can make a difference to\nthe state of the environment")
  pC <- plotFun(d1, "Env.ClimateChgReal.T11"   , " \n\n \nClimate change is real")
  pD <- plotFun(d1, "Env.ClimateChgCause.T11"  , " \n\n \nClimate change is caused by humans")
  pE <- plotFun(d1, "Env.ClimateChgConcern.T11", " \n\n \nI am deeply concerned about climate change")
  pF <- plotFun(d1, "Env.SacMade.T09"          , "\nHave you made sacrifices to\nyour standard of living in order\nto protect the environment?")
  pG <- plotFun(d1, "Env.SacWilling.T09"       , "Are you willing to \nmake sacrifices to your\n standard of living in\norder to protect the environment?")
  pH <- plotFun(d1, "Env.SacNorms.T09"         , "Do you think most New Zealanders \nare willing to make sacrifices to \ntheir standard of living \nin order to protect the environment?")
  # put together
  top <- plot_grid(pA + theme(legend.position = "none"),
                   pB + theme(legend.position = "none"),
                   pC + theme(legend.position = "none"),
                   pD + theme(legend.position = "none"), nrow = 1, labels = letters[1:4])
  bot <- plot_grid(pE + theme(legend.position = "none"),
                   pF + theme(legend.position = "none"),
                   pG + theme(legend.position = "none"),
                   pH + theme(legend.position = "none"), nrow = 1, labels = letters[5:8])
  out <- plot_grid(top, bot, nrow = 2)
  out <- plot_grid(out, get_legend(pA), rel_widths = c(1, 0.1))
  # add common x-axis
  x.grob <- textGrob("Trust Game", gp = gpar(fontsize = 10))
  out <- grid.arrange(arrangeGrob(out, bottom = x.grob))
  out <- ggdraw(out)
  # save
  ggsave(out, filename = "figures/plotTrust.pdf", width = 10, height = 6)
  return(out)
}

# plot comp world
plotCompWorld1 <- function(d1) {
  # generic plotting function
  plotFun <- function(d, dv, ylab) {
    d %>%
      mutate(compWorld = as.numeric(scale( ((Comp.World01.T06 + Comp.World02r.T06) / 2) ))) %>%
      drop_na() %>%
      ggplot(aes(x = compWorld, y = !!sym(dv))) +
      geom_jitter(height = 0.3, alpha = 0.05, size = 0.5) +
      geom_smooth(colour = "black", method = "lm") +
      scale_y_continuous(name = ylab, breaks = 1:7) +
      labs(x = NULL) +
      theme_classic() +
      theme(axis.title.y = element_text(size = 9))
  }
  # separate plots
  pA <- plotFun(d1, "Env.Eff01.T09"            , "\nBy taking personal action I \nbelieve I can make a positive \ndifference to environmental problems")
  pB <- plotFun(d1, "Env.Eff02.T09"            , " \n\nI feel I can make a difference to\nthe state of the environment")
  pC <- plotFun(d1, "Env.ClimateChgReal.T11"   , " \n\n \nClimate change is real")
  pD <- plotFun(d1, "Env.ClimateChgCause.T11"  , " \n\n Climate change is\n caused by humans")
  pE <- plotFun(d1, "Env.ClimateChgConcern.T11", " \n\nI am deeply concerned \nabout climate change")
  pF <- plotFun(d1, "Env.SacMade.T09"          , "\nHave you made sacrifices to\nyour standard of living in order\nto protect the environment?")
  pG <- plotFun(d1, "Env.SacWilling.T09"       , "Are you willing to \nmake sacrifices to your\n standard of living in\norder to protect the environment?")
  pH <- plotFun(d1, "Env.SacNorms.T09"         , "Do you think most New \nZealanders are willing to make \nsacrifices to their standard of \nliving in order to protect\n the environment?")
  # put together
  top <- plot_grid(pA + theme(legend.position = "none"),
                   pB + theme(legend.position = "none"),
                   pC + theme(legend.position = "none"), nrow = 1, labels = letters[1:3])
  mid <- plot_grid(pD + theme(legend.position = "none"),
                   pE + theme(legend.position = "none"),
                   pF + theme(legend.position = "none"), nrow = 1, labels = letters[4:6])
  bot <- plot_grid(NULL,
                   pG + theme(legend.position = "none"),
                   pH + theme(legend.position = "none"), 
                   NULL, nrow = 1, labels = c("", "g", "h", ""), rel_widths = c(0.5, 1, 1, 0.5))
  out <- plot_grid(top, mid, bot, nrow = 3)
  # add common x-axis
  x.grob <- textGrob("Competitive Worldview (std)", gp = gpar(fontsize = 10))
  out <- grid.arrange(arrangeGrob(out, bottom = x.grob))
  out <- ggdraw(out)
  # save
  ggsave(out, filename = "figures/plotCompWorld.pdf", width = 7, height = 7)
  return(out)
}