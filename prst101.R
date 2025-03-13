library(marinecs100b)
library(tidyverse)


# Frequency of occurrence -------------------------------------------------

# P1 Filter cctd_meso to rows representing the predator with id 99112. What was
# species was the predator? How many prey items were in their diet sample? How
# many of those prey were mesopelagic or coastal pelagic fish species?

cctd_meso[cctd_meso$predator_id == 99112, ]


View(cctd_meso)
find_predator <- function() {
index = c()

for (n in 1:nrow(cctd_meso)) {

      if (cctd_meso[n,1] == 99112) {
      index <- c(index, n)
        }

  }
return(index)
}
# CTRL SHIFT C


#The predator is a califonia sea lion, it has 5 prey in its diet, 1 meso, 3 coastal

# P2 Summarize cctd_meso at the predator level (i.e., each row in the summary
# should represent one predator), indicating whether the predator’s diet sample
# contained mesopelagic and/or coastal pelagic fish species. Call the summary
# columns any_meso and any_cpf.

# ?any()
# summarize()

summarized_predator <- cctd_meso %>%
  group_by(predator_id, predator_common_name) %>%
  summarize(any_meso = any(meso_prey),
            any_cpf = any(cpf_prey),
            .groups = "drop")

View(summarized_predator)

# make_predator_small <- function() {
#   index = c()
#
#
#   for (n in 1:nrow(cctd_meso)) {
#     is_unique <- TRUE
#
#     for(j in unique) {
#
#       if (j == cctd_meso[n, 1]) {
#
#         is_unique <- FALSE
#         break
#
#       }
#
#     }
#     if (is_unique) {
#       unique <- c(unique, cctd_meso[n, 1])
#     }
#   }
#   return(unique)
# }
#
# make_predator_small()



# P3 Using the data frame you created in P2, create a species-level summary that
# contains columns for mesopelagic FO (meso_fo), coastal pelagic fish FO
# (cpf_fo), and predator sample size (n).


#Group by species, so go back to P2 to change

?mean()

summarized_predator_species <- summarized_predator %>%
  group_by(predator_common_name) %>%
  summarize(
            meso_fo = mean(any_meso),
            cpf_fo = mean(any_cpf),
            n = n())
            #n = sum(predator_id/predator_id))

View(summarized_predator_species)

# summarized_predator_species1 <- cctd_meso %>%
#   group_by(predator_common_name) %>%
#   summarize(meso_fo = sum(cpf_prey))
#
# summarized_predator_species2 <- cctd_meso %>%
#   group_by(prey_common_name) %>%
#   summarize(meso_fo = sum(prey_id/prey_id))
#
# cctd_meso1 <- sum
#
# ?summarize
# ?sum()



# P4 How many predator species had a mesopelagic FO greater than 0.5? Which of
# those predator species had the largest sample size?

summarized_predator_species[summarized_predator_species$meso_fo > 0.5, ]

# There was 6 species!
# Humbolt squid


summarized_predator_species %>%
  # Only retain predator species that ate mesopelagic fish
  filter(meso_fo > 0) %>%
  # Flip sign of coastal pelagic FO (for plotting purposes)
  mutate(cpf_fo = -cpf_fo,
         # Format name and sample size
         label = sprintf("%s (%d)", predator_common_name, n),
         # Order labels by descending mesopelagic FO
         label = fct_reorder(label, meso_fo)) %>%
  # Rename *_fo columns to human-readable
  rename(`Coastal pelagic fish` = cpf_fo,
         `Mesopelagic fish` = meso_fo) %>%
  # Pivot frequency of occurence columns to long format
  pivot_longer(ends_with("Fish"), values_to = "fo") %>%
  # Create column chart
  ggplot(aes(x = fo, y = label, fill = name)) +
  geom_col(color = "grey10") +
  scale_x_continuous("Frequency of occurrence", limits = c(-1, 1)) +
  labs(y = "Predator taxa") +
  scale_fill_manual("Prey type", values = c("grey70", "navy")) +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"),
        legend.position = "inside",
        legend.position.inside = c(0.99, 0.01),
        legend.justification = c(1, 0))
ggsave("meso_cpf_fo.png", width = 8, height = 6, units = "in")

# Simulating samples ------------------------------------------------------

set.seed(123)

# P5 In the sample, Lissodelphis had a slightly higher mesopelagic FO than
# Delphinus. Do you think that relative order reflects the true difference in
# the population? Why or why not?
# I would have to do a t/p-test in otder to determine if there is a 95% confidence there is a significant difference, but i would say that the sample size form a glance shows there's not a significant diff.

# P6 Fill in the blanks below to simulate 1000 new samples. Keep the size and
# probabilities the same as the original sample. Of the 1000 simulated samples,
# what fraction show the wrong order (i.e., mesopelagic FO greater in Delphinus
# than Lissodelphis)?

# summarized_predator_species <- summarized_predator %>%
#   group_by(predator_common_name) %>%
#   summarize(
#     meso_fo = mean(any_meso),
#     cpf_fo = mean(any_cpf),
#     n = sum(predator_id/predator_id))

lissodelphis_samples <- rbinom(1000,
                               size = 56,
                               prob = 0.8928571429)

lissodelphis_fo <- data.frame(lissodelphis_samples)

lissodelphis_fo1 <- lissodelphis_fo %>%
  group_by(lissodelphis_samples) %>%
  summarize(
    lissodelphis_fo = (lissodelphis_samples/56)
  )

View(lissodelphis_fo1)


delphinus_samples <- rbinom(1000,
                            size = 259,
                            prob = 0.8571428571)

delphinus_fo <- data.frame(delphinus_samples)

delphinus_fo1 <- delphinus_fo %>%
  group_by(delphinus_samples) %>%
  summarize(
    delphinus_fo = (delphinus_samples/259)
  )
View(delphinus_fo1)

wrong_order = 0
wrong_order <- lissodelphis_fo1$lissodelphis_fo < delphinus_fo1$delphinus_fo
sum(wrong_order)
# 43 samples!


# index = 0
# for (n in 1:nrow(lissodelphis_fo1)) {
#
#   if (lissodelphis_fo1[n,2] > delphinus_fo1[n,2]) {
#     index <- index + 1
#   }
#
# }
# wrong_order <- index/1000

# P7 How does your result in P6 influence your confidence in the sample result
# that Lissodelphis consume mesopelagic prey more frequently than Delphinus?

# This statistics test shows there's a ~95% confidence the difference is not due to chance, which improves my confidence in the consumption diff.

# P8 If you were to get new samples of the same size for these two taxa, which
# mesopelagic FO do you think would change more? Why?

# The lissodelphis_fo would change more due to the smaller sample size.

# P9 Generate 1000 new simulated samples for Histioteuthidae and Dosidicus
# gigas, keeping the sample sizes and probabilities the same.

Histioteuthidae_samples <- rbinom(1000,
                            size = 47,
                            prob = 0.5531914894)

Histioteuthidae_fo <- data.frame(Histioteuthidae_samples)
View(Dosidicus_samples)


Dosidicus_samples <- rbinom(1000,
                                  size = 1136,
                                  prob = 0.5220070423)

Dosidicus_fo <- data.frame(Dosidicus_samples)

# P10 What’s the mean mesopelagic FO of the 1000 Histioteuthidae simulated
# samples? How about Dosidicus gigas? How do these means compare to the original
# sample?

Histioteuthidae_fo1 <- Histioteuthidae_fo %>%
  group_by(Histioteuthidae_samples) %>%
  summarize(
    Histioteuthidae_fo = (Histioteuthidae_samples/47),
  )

View(Histioteuthidae_fo1)

Dosidicus_fo1 <- Dosidicus_fo %>%
  group_by(Dosidicus_samples) %>%
  summarize(
    Dosidicus_fo = (Dosidicus_samples/1136)
  )

temp <- sum(Dosidicus_fo1$Dosidicus_fo)
Dosidicus_mean <- temp/1000

temp1 <- sum(Histioteuthidae_fo1$Histioteuthidae_fo)
Histioteuthidae_mean <- temp1/1000

# P11 What’s the standard deviation of mesopelagic FO across simulated samples
# for the two taxa?

m <- Dosidicus_fo1$Dosidicus_fo
sd(m)
# 0.01451915 Dosidicus SD

m1 <- Histioteuthidae_fo1$Histioteuthidae_fo
sd(m1)
# 0.07169056 Histioteuthidae SD


# P12 How frequently did Histioteuthidae mesopelagic FO fall outside the range
# 0.45 - 0.65? How about Dosidicus gigas?

M <- sum(Histioteuthidae_fo1$Histioteuthidae_fo < 0.45 | Histioteuthidae_fo1$Histioteuthidae_fo > 0.65)

# index2 <- 0
# for (n in 1:nrow(Histioteuthidae_fo1)) {
#
#   if (Histioteuthidae_fo1[n,2] > 0.65 | Histioteuthidae_fo1[n,2] < 0.45) {
#     index2 <- index2 + 1
#   }
#
# }

# Histioteuthidae 177

N <- sum(Dosidicus_fo1$Dosidicus_fo < 0.45 | Dosidicus_fo1$Dosidicus_fo > 0.65)

# index3 <- 0
# for (n in 1:nrow(Dosidicus_fo1)) {
#
#   if (Dosidicus_fo1[n,2] > 0.65 | Dosidicus_fo1[n,2] < 0.45) {
#     index3 <- index3 + 1
#   }
# }

# Dosidicus ZERO

# P13 Based on your answers to P10-P12, what effect does sample size have on
# sample accuracy?
# A higher sample size means a higher accuracy for sure!
