###################
#### Read Data ####
###################

library(tidyverse)

houses <- suppressMessages(read_csv("JSE13-070R2.csv"))

# for simplicity, combine full and half baths
houses$no_baths <- houses$no_full_baths + (0.5 * houses$no_half_baths)

houses$zip <- factor(houses$zip)

houses$bedrooms <- factor(ifelse(houses$bedrooms > 2, 
                                 "3+", houses$bedrooms))
houses$garage_spaces <- factor(ifelse(houses$garage_spaces > 1, 
                                      "2+", houses$garage_spaces))

houses <- houses[, c("acre", "adj2007", "bedrooms", "bedgroup", "bikescore", 
                     "walkscore", "distance", "garage_spaces", "no_rooms", 
                     "no_baths", "squarefeet", "zip")]

colnames(houses) <- c("acre", "home_value", "bedrooms", "bedgroup", "bikescore", 
                      "walkscore", "distance", "garage_spaces", "n_rooms", 
                      "n_baths", "squarefeet", "zip")

fit <- lm(log(home_value) ~ log(distance) + bedrooms + garage_spaces + n_baths + 
              zip + squarefeet, data = houses)

summary(fit)
plot(fit)

save(houses, fit, file = "data.rda")