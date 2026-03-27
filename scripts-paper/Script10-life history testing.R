library(readxl); library(dplyr)
options(scipen = 4)

fa <- read_excel("1b-reps-10-hypothtest.xlsx")
fasub <- filter(fa, Teeth != "Other")
fasubf <- filter(fa, Sex == "Female")
fasubm <- filter(fa, Sex == "Male")

with(fasub, t.test(FA10[Teeth == "M1"], FA10[Teeth == "M3"]))
with(fasubf, t.test(FA10[Teeth == "M1"], FA10[Teeth == "M3"]))
with(fasubm, t.test(FA10[Teeth == "M1"], FA10[Teeth == "M3"]))
