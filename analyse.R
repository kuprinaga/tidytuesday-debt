library(tidyverse)
library(tidytuesdayR)

df <- tt_load(2019, week = 48)

loans <- df[[1]]

loans %>%
  mutate(label = stringr::str_c(year, ' Q', quarter)) %>%
  group_by(label) %>%
  summarise(total_debt = sum(total, na.rm = T),
            total_defaulted_removed = sum(rehabilitation, consolidation, na.rm = T),
            total_payments = sum(voluntary_payments, wage_garnishments, na.rm = T),
            total_voluntary = sum(voluntary_payments, na.rm = T),
            total_salary = sum(wage_garnishments, na.rm = T)) %>%
  mutate(to_be_paid = total_debt - total_defaulted_removed,
         difference = to_be_paid - total_payments)

loans %>%
  ggplot(aes(x = label,
             y = total_debt)) +
  geom_point() +
  geom_area(aes(group = 1),
            alpha = 0.3)

loans %>%
  ggplot(aes(x = label,
             y = total)) +
  geom_point() +
  geom_area(aes(group = 1),
            alpha = 0.3)
