library(palmerpenguins)
library(tidyverse)

penguins

#examples of summary stats using groupby() and summarize()

#ex one group, one summary statistic
penguins %>%
  group_by(species) %>%
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

#one group, 2 summary
penguins %>%
  group_by(species) %>%
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            sd_bill_length = sd(bill_length_mm, na.rm = TRUE))

#2 group, 1 summarypenguins
penguins %>%
group_by(species, sex) %>%
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            sd_bill_length = sd(bill_length_mm, na.rm = TRUE))

#also clean up groups

penguins %>%
  group_by(species) %>%
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            sd_bill_length = sd(bill_length_mm, na.rm = TRUE),
            .groups = "drop")

rbinom(n=1, size=1, prob = 0.5)
rbinom(n=10, size=1, prob = 0.5)
rbinom(n=101, size=n_flips, prob = 1.000000 / n_flips)
