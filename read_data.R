###################
#### Read Data ####
###################

library(tidyverse)

houses <- suppressMessages(read_csv("JSE13-070R2.csv"))

# for simplicity, combine full and half baths
houses$no_baths <- houses$no_full_baths + (0.5 * houses$no_half_baths)

houses$zip <- factor(houses$zip)

houses <- houses[, c("acre", "adj2007", "bedrooms", "bedgroup", "bikescore", 
                     "walkscore", "distance", "garage_spaces", "no_rooms", 
                     "no_baths", "squarefeet", "zip")]

fit <- lm(log(adj2007) ~ log(distance) + bedgroup + garage_spaces + no_baths + zip + 
               squarefeet, data = houses)

save(houses, fit, file = "data.rda")
