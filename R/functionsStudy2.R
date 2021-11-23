# functions for study 2

# load data
loadData2 <- function() {
  # isocodes
  iso <-
    read.csv("data/study2/isocodes/countries_codes_and_coordinates.csv") %>%
    transmute(iso3  = gsub(" ", "", Alpha.3.code), 
              iso2  = gsub(" ", "", Alpha.2.code)) %>%
    distinct()
  # human development index
  hdi <- read.csv("data/study2/controls/hdi.csv")
  # predictor - world values survey
  d1 <- 
    readRDS("data/study2/predictors/wvs/EVS_WVS_Joint_v2_0.rds") %>%
    group_by(cntry_AN) %>%
    summarise(wvsPropTrust = sum(A165 == 1, na.rm = TRUE) / sum(A165 %in% 1:2, na.rm = TRUE)) %>%
    rename(iso2 = cntry_AN) %>%
    left_join(iso, by = "iso2")
  # predictor - gps
  d2 <- 
    read_dta("data/study2/predictors/gps/country_v11.dta") %>%
    rename(iso3 = isocode) %>%
    left_join(iso, by = "iso3")
  # outcome - co2
  d3 <- 
    read.csv("data/study2/outcomes/co2-emissions-and-gdp.csv") %>%
    rename(iso3 = Code) %>%
    left_join(iso, by = "iso3")
  # join datasets
  out <-
    d1 %>%
    full_join(d2, by = c("iso2", "iso3")) %>%
    left_join(d3, by = c("iso2", "iso3")) %>%
    left_join(hdi, by = "iso2") %>%
    # remove three countries not in religious proximity matrix - HK, MO, PR
    # and one country without HDI value - TW
    filter(!(iso2 %in% c("HK", "MO", "PR", "TW")))
  return(out)
}

# load proximity matrix
loadProximityMatrix <- function(d2, file) {
  # load proximity matrix
  out <- 
    read.csv(file, row.names = 1, na.strings = "") %>%
    as.matrix()
  # keep only countries from gps
  out <- out[rownames(out) %in% d2$iso2,
             colnames(out) %in% d2$iso2]
  # diagonal
  diag(out) <- 1
  return(out)
}

# fit country-level c02 model
fitModel2 <- function(d2, geoProximity, lingProximity, reliProximity, 
                      predictors = FALSE, formula) {
  # modify year (0 - 1)
  d2$Year <- d2$Year - min(d2$Year)
  d2$Year <- d2$Year / max(d2$Year)
  # create copies of iso2
  d2$iso2a <- d2$iso2
  d2$iso2b <- d2$iso2
  d2$iso2c <- d2$iso2
  # prior
  prior <- c(prior(normal(0, 1), class = Intercept),
             prior(exponential(4), class = sd))
  if (predictors) prior <- c(prior, prior(normal(0, 0.5), class = b))
  # fit model
  out <- brm(formula = formula, data = d2, family = exponential,
             data2 = list(geo  = geoProximity,
                          ling = lingProximity,
                          reli = reliProximity),
             iter = 4000, control = list(adapt_delta = 0.99), 
             cores = 4, seed = 2113)
  out <- add_criterion(out, "loo")
  return(out)
}

# plot predictions across countries
plotPredCountry2 <- function(model) {
  # new data
  new <-
    expand_grid(
      Year = seq(0, 1, length.out = 101),
      iso2 = unique(model$data$iso2)
    ) %>%
    mutate(
      iso2a = iso2,
      iso2b = iso2,
      iso2c = iso2
    )
  # fitted values
  f <- cbind(new, fitted(model, newdata = new))
  # data
  out <-
    ggplot() +
    geom_point(data = model$data, aes(x = Year, y = Per.capita.CO2.emissions), size = 0.5) +
    geom_ribbon(data = f, aes(x = Year, ymin = Q2.5, ymax = Q97.5), fill = "grey", alpha = 0.6) +
    geom_line(data = f, aes(x = Year, y = Estimate), colour = "blue") +
    facet_wrap(. ~ iso2, ncol = 10) +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("1990", "2005", "2019")) +
    ylab("Per capita C02 emissions (tonnes)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  ggsave(out, filename = "figures/plotPred.pdf", height = 11, width = 10)
  return(out)
}

# plot average predictions by trust levels
plotPredTrust2 <- function(modelWVS, modelGPS, file) {
  # world values survey
  condA <- 
    conditional_effects(
      modelWVS, effect = "Year:wvsPropTrust", prob = 0.50,
      int_conditions = list(wvsPropTrust = c(0.02, 0.26, 0.77)))
  pA <-
    plot(condA, plot = FALSE)[[1]] +
    scale_y_continuous(name = "Per capita C02 emissions (tonnes)",
                       limits = c(0, 22)) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                       labels = c("1990", "1998", "2005", "2012", "2019")) +
    ggtitle("WVS Trust") +
    theme_classic() +
    theme(legend.position = "none")
  # global preferences survey
  condB <- 
    conditional_effects(
      modelGPS, effect = "Year:trust", prob = 0.50,
      int_conditions = list(trust = c(-0.71, -0.02, 0.61)))
  pB <-
    plot(condB, plot = FALSE)[[1]] +
    scale_y_continuous(name = " ", limits = c(0, 22)) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                       labels = c("1990", "1998", "2005", "2012", "2019")) +
    scale_fill_discrete(name = "Trust", labels = c("Max", "Mean", "Min")) +
    scale_colour_discrete(name = "Trust", labels = c("Max", "Mean", "Min")) +
    ggtitle("GPS Trust") +
    theme_classic()
  # put together
  out <- plot_grid(pA, pB, nrow = 1, rel_widths = c(0.75, 1))
  ggsave(out, filename = file, width = 7, height = 4)
  return(out)
}

plotPredHDI2 <- function(modelWVS, modelGPS, file) {
  # world values survey
  condA <- 
    conditional_effects(
      modelWVS, effect = "Year:hdi", prob = 0.50,
      int_conditions = list(hdi = c(0.48, 0.78, 0.96)))
  pA <-
    plot(condA, plot = FALSE)[[1]] +
    scale_y_continuous(name = "Per capita C02 emissions (tonnes)",
                       limits = c(0, 22)) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                       labels = c("1990", "1998", "2005", "2012", "2019")) +
    ggtitle("WVS") +
    theme_classic() +
    theme(legend.position = "none")
  # global preferences survey
  condB <- 
    conditional_effects(
      modelGPS, effect = "Year:hdi", prob = 0.50,
      int_conditions = list(trust = c(0.48, 0.78, 0.96)))
  pB <-
    plot(condB, plot = FALSE)[[1]] +
    scale_y_continuous(name = " ", limits = c(0, 22)) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                       labels = c("1990", "1998", "2005", "2012", "2019")) +
    scale_fill_discrete(name = "HDI", labels = c("Max", "Mean", "Min")) +
    scale_colour_discrete(name = "HDI", labels = c("Max", "Mean", "Min")) +
    ggtitle("GPS") +
    theme_classic()
  # put together
  out <- plot_grid(pA, pB, nrow = 1, rel_widths = c(0.75, 1))
  ggsave(out, filename = file, width = 7, height = 4)
  return(out)
}

# load sufficiency data
loadData3 <- function() {
  # isocodes
  iso <-
    read.csv("data/study2/isocodes/countries_codes_and_coordinates.csv") %>%
    transmute(iso3  = gsub(" ", "", Alpha.3.code), 
              iso2  = gsub(" ", "", Alpha.2.code)) %>%
    distinct()
  # human development index
  hdi <- read.csv("data/study2/controls/hdi.csv")
  # predictor - world values survey
  d1 <- 
    readRDS("data/study2/predictors/wvs/EVS_WVS_Joint_v2_0.rds") %>%
    group_by(cntry_AN) %>%
    summarise(wvsPropTrust = sum(A165 == 1, na.rm = TRUE) / sum(A165 %in% 1:2, na.rm = TRUE)) %>%
    rename(iso2 = cntry_AN) %>%
    left_join(iso, by = "iso2")
  # predictor - gps
  d2 <- 
    read_dta("data/study2/predictors/gps/country_v11.dta") %>%
    rename(iso3 = isocode) %>%
    left_join(iso, by = "iso3")
  # outcome - sufficiency
  d3 <- 
    read_xlsx("data/study2/outcomes/Sufficiency Levels.xlsx") %>%
    mutate(sufficiency = 6 - sufficiency)
  # join datasets
  out <-
    d1 %>%
    full_join(d2, by = c("iso2", "iso3")) %>%
    full_join(d3, by = "iso2") %>%
    left_join(hdi, by = "iso2") %>%
    # remove three countries not in religious proximity matrix - HK, MO, PR
    # and one country without HDI value - TW
    filter(!(iso2 %in% c("HK", "MO", "PR", "TW")))
  return(out)
}

# fit country-level sufficiency model
fitModel3 <- function(d5, geoProximity, lingProximity, reliProximity, 
                      predictors = FALSE, formula) {
  # create copies of iso2
  d5$iso2a <- d5$iso2
  d5$iso2b <- d5$iso2
  d5$iso2c <- d5$iso2
  # prior
  prior <- c(prior(normal(0, 2), class = Intercept),
             prior(exponential(4), class = sd))
  if (predictors) prior <- c(prior, prior(normal(0, 1), class = b))
  # fit model
  out <- brm(formula = formula, data = d5, family = cumulative,
             data2 = list(geo  = geoProximity,
                          ling = lingProximity,
                          reli = reliProximity),
             iter = 4000, control = list(adapt_delta = 0.99), 
             cores = 4, seed = 2113)
  out <- add_criterion(out, "loo")
  return(out)
}

# plot sufficiency models
plotPredSufficiency2 <- function(m5b, m5d, m6b, m6d) {
  # conditional effects
  pA <- conditional_effects(m5b, effects = "wvsPropTrust", categorical = TRUE, prob = 0.50)
  pB <- conditional_effects(m5d, effects = "wvsPropTrust", categorical = TRUE, prob = 0.50)
  pC <- conditional_effects(m6b, effects = "trust", categorical = TRUE, prob = 0.50)
  pD <- conditional_effects(m6d, effects = "trust", categorical = TRUE, prob = 0.50)
  # plotting function
  plotFun <- function(cond, xlab, title) {
    plot(cond, plot = FALSE)[[1]] +
      scale_x_continuous(name = xlab) +
      scale_y_continuous(limits = c(0, 1)) +
      scale_colour_discrete(name = NULL, 
                            labels = c("Critically insufficient", 
                                       "Highly insufficient", 
                                       "Insufficient", 
                                       "Almost sufficient")) +
      scale_fill_discrete(name = NULL,
                          labels = c("Critically insufficient", 
                                     "Highly insufficient", 
                                     "Insufficient", 
                                     "Almost sufficient")) +
      ggtitle(title) +
      theme_classic()
  }
  # plots
  pA <- plotFun(pA, xlab = "WVS Trust (prop)", title = "No controls")
  pB <- plotFun(pB, xlab = "WVS Trust (prop)", title = "HDI control")
  pC <- plotFun(pC, xlab = "GPS Trust (std)", title = "No controls")
  pD <- plotFun(pD, xlab = "GPS Trust (std)", title = "HDI control")
  # put together
  top <- plot_grid(pA + theme(legend.position = "none"),
                   pB + theme(legend.position = "none"), nrow = 1, labels = c("a","b"))
  bot <- plot_grid(pC + theme(legend.position = "none"),
                   pD + theme(legend.position = "none"), nrow = 1, labels = c("c","d"))
  out <- plot_grid(top, bot, nrow = 2)
  out <- plot_grid(out, get_legend(pA), nrow = 1, rel_widths = c(1, 0.35))
  # save
  ggsave(out, file = "figures/plotSufficiency.pdf", width = 6.5, height = 5)
  return(out)
}

# load pew data
loadData4 <- function() {
  # isocodes
  iso <-
    read.csv("data/study2/isocodes/countries_codes_and_coordinates.csv") %>%
    transmute(iso3  = gsub(" ", "", Alpha.3.code), 
              iso2  = gsub(" ", "", Alpha.2.code)) %>%
    distinct()
  # human development index
  hdi <- read.csv("data/study2/controls/hdi.csv")
  # predictor - world values survey
  d1 <- 
    readRDS("data/study2/predictors/wvs/EVS_WVS_Joint_v2_0.rds") %>%
    group_by(cntry_AN) %>%
    summarise(wvsPropTrust = sum(A165 == 1, na.rm = TRUE) / sum(A165 %in% 1:2, na.rm = TRUE)) %>%
    rename(iso2 = cntry_AN) %>%
    left_join(iso, by = "iso2")
  # predictor - gps
  d2 <- 
    read_dta("data/study2/predictors/gps/country_v11.dta") %>%
    rename(iso3 = isocode) %>%
    left_join(iso, by = "iso3")
  # outcome - pew data
  d3 <- 
    read_xlsx("data/study2/outcomes/Pew Study.xlsx") %>%
    mutate_at(vars(starts_with("perc")), function(x) x / 100) %>%
    rename_at(vars(starts_with("perc")), function(x) str_replace(x, "perc", "prop"))
  # join datasets
  out <-
    d1 %>%
    full_join(d2, by = c("iso2", "iso3")) %>%
    full_join(d3, by = "iso2") %>%
    left_join(hdi, by = "iso2") %>%
    # remove three countries not in religious proximity matrix - HK, MO, PR
    # and one country without HDI value - TW
    filter(!(iso2 %in% c("HK", "MO", "PR", "TW")))
  return(out)
}

# fit country-level pew model
fitModel4 <- function(d6, geoProximity, lingProximity, reliProximity, 
                      predictors = FALSE, formula) {
  # create copies of iso2
  d6$iso2a <- d6$iso2
  d6$iso2b <- d6$iso2
  d6$iso2c <- d6$iso2
  # prior
  prior <- c(prior(normal(0, 1), class = Intercept),
             prior(exponential(4), class = sd),
             prior(exponential(0.1), class = phi))
  if (predictors) prior <- c(prior, prior(normal(0, 0.5), class = b))
  # fit model
  out <- brm(formula = formula, data = d6, family = Beta,
             data2 = list(geo  = geoProximity,
                          ling = lingProximity,
                          reli = reliProximity),
             iter = 3000, control = list(adapt_delta = 0.999), 
             cores = 4, seed = 2113)
  out <- add_criterion(out, "loo")
  return(out)
}

# plot pew models
plotPredPew2 <- function(m7b, m7d, m8b, m8d) {
  # conditional effects
  pA <- conditional_effects(m7b, effects = "wvsPropTrust", prob = 0.95)
  pB <- conditional_effects(m7d, effects = "wvsPropTrust", prob = 0.95)
  pC <- conditional_effects(m8b, effects = "trust", prob = 0.95)
  pD <- conditional_effects(m8d, effects = "trust", prob = 0.95)
  # plotting function
  plotFun <- function(cond, xlab, title) {
    plot(cond, plot = FALSE, points = TRUE)[[1]] +
      scale_x_continuous(name = xlab) +
      scale_y_continuous(name = "Proportion of people thinking that\nglobal climate change is a major threat", 
                         limits = c(0, 1)) +
      ggtitle(title) +
      theme_classic()
  }
  # plots
  pA <- plotFun(pA, xlab = "WVS Trust (prop)", title = "No controls")
  pB <- plotFun(pB, xlab = "WVS Trust (prop)", title = "HDI control")
  pC <- plotFun(pC, xlab = "GPS Trust (std)", title = "No controls")
  pD <- plotFun(pD, xlab = "GPS Trust (std)", title = "HDI control")
  # put together
  top <- plot_grid(pA + theme(legend.position = "none"),
                   pB + theme(legend.position = "none"), nrow = 1, labels = c("a","b"))
  bot <- plot_grid(pC + theme(legend.position = "none"),
                   pD + theme(legend.position = "none"), nrow = 1, labels = c("c","d"))
  out <- plot_grid(top, bot, nrow = 2)
  # save
  ggsave(out, file = "figures/plotPew.pdf", width = 7, height = 7)
  return(out)
}