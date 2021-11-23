library(dplyr)
# world values survey
dWVS <- readRDS("data/wvs/WVS_Cross-National_Wave_7_R_v1_6.rds")
#is a yes or no question 

# global preferences survey
dGPScountry    <- readstata13::read.dta13("data/gps/country.dta")
dGPSindividual <- readstata13::read.dta13("data/gps/individual_new.dta")

dGPScountry_subset <- select(dGPScountry, country, trust)
dGPScountry_subset <- arrange(dGPScountry_subset, desc(trust))

#this is how you grab out columns you want

grep("Q57", names(dWVS), value = TRUE)
grep("COUNTRY", names(dWVS), value = TRUE)
#^This is how you see if the column is there (t/f)

## Number of rows (ie number of people in WVS survey)
nrow(dWVS)

table(dWVS$B_COUNTRY_ALPHA)


dWVS_subset <- select(dWVS, B_COUNTRY_ALPHA, Q57)
#this is how you grab columns you want

table(dWVS_subset$Q57, exclude = NULL)
#remove questions (unanswered)


dWVS_average <- dWVS_subset %>%
  filter(!is.na(Q57)) %>% 
  group_by(B_COUNTRY_ALPHA) %>% 
  summarise(
    Q57_total_yes = sum(Q57 == 1),
    Q57_total_answers = n(),
    Q57_proportion = Q57_total_yes/Q57_total_answers
  )


dWVS_average <- arrange(dWVS_average, desc(Q57_total_yes))

write.csv(dWVS_average, file = "dwVs_average.csv")
write.csv(dGPScountry_subset, file = "dGPScountry_subset.csv")

sort(setdiff(dWVS_average$B_COUNTRY_ALPHA, dGPScountry$isocode))
sort(setdiff(dGPScountry$isocode, dWVS_average$B_COUNTRY_ALPHA))

intersect(
  unique(dWVS$B_COUNTRY_ALPHA),
  unique(dGPScountry$isocode)
)

#looking for level of trust for each country 

#characterisitics

View(unique(dWVS[, c("B_COUNTRY_ALPHA", "medageun")]))

table(dWVS$B_COUNTRY_ALPHA)

