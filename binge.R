library(readxl)
library(tidyverse)
library(dplyr)

f <- "SRTX2_SubjectData.xlsx"
d <- read_excel(f, sheet = 3, col_names = TRUE)

### Intox -------------------------------------
e <- read_excel(f, sheet = 4, col_names = TRUE)
e <- e[-1:-2, 1:15]
e[ e == "Sac" ] <- NA
e[ e == "N/A" ] <- NA
e <- na.omit(e)
e[4:15] <- sapply(e[4:15],as.numeric)

names(e) <- c("subject", "group", "sex", "day1_1", 
              "day1_2", "day1_3", "day2_1", "day2_2", 
              "day2_3", "day3_1", "day3_2", "day3_3",
              "day4_1", "day4_2", "day4_3")

z <- e %>% group_by(sex) %>% summarise(
  day1_1 = mean(day1_1),
  day1_2 = mean(day1_2),
  day1_3 = mean(day1_3),
  day2_1 = mean(day2_1),
  day2_2 = mean(day2_2),
  day2_3 = mean(day2_3),
  day3_1 = mean(day3_1),
  day3_2 = mean(day3_2),
  day3_3 = mean(day3_3),
  day4_1 = mean(day4_1),
  day4_2 = mean(day4_2),
  day4_3 = mean(day4_3)
) 
z1 <- z %>% pivot_longer(cols = starts_with("day"), names_to = "time", values_to = "score")

ggplot(data = z1, aes(x = as.factor(time), y = score, color = sex)) +
  geom_point() +
  geom_line(data = z1, aes(group = sex))


z2 <- z %>% group_by(sex) %>% summarise(
  day1 = mean(day1_1 + day1_2 + day1_3),
  day2 = mean(day2_1 + day2_2 + day2_3),
  day3 = mean(day3_1 + day3_2 + day3_3),
  day4 = mean(day4_1 + day4_2 + day4_3)
)

z2 <- z2 %>% pivot_longer(cols = starts_with("day"), names_to = "time", values_to = "score")

ggplot(data = z2, aes(x = as.factor(time), y = score, color = sex)) +
  geom_point() +
  geom_line(data = z2, aes(group = sex))

z3 <- e %>% mutate(
  max = do.call(pmax, e[, 4:15])
)

z3 <- z3 %>% group_by(sex) %>% summarize(
  max = mean(max)
)

ggplot(data = z3, aes(x = sex, y = max)) +
  geom_bar(stat = "identity")


### Dose -------------------------------------
g <- read_excel(f, sheet = 5, col_names = TRUE)
g <- g[-1:-2, 1:15]
g[ g == "Sac" ] <- NA
g[ g == "N/A" ] <- NA
g <- na.omit(g)
g[4:15] <- sapply(g[4:15],as.numeric)

names(g) <- c("subject", "group", "sex", "day1_1", 
              "day1_2", "day1_3", "day2_1", "day2_2", 
              "day2_3", "day3_1", "day3_2", "day3_3",
              "day4_1", "day4_2", "day4_3")

y <- g %>% group_by(sex) %>% summarise(
  day1_1 = mean(day1_1),
  day1_2 = mean(day1_2),
  day1_3 = mean(day1_3),
  day2_1 = mean(day2_1),
  day2_2 = mean(day2_2),
  day2_3 = mean(day2_3),
  day3_1 = mean(day3_1),
  day3_2 = mean(day3_2),
  day3_3 = mean(day3_3),
  day4_1 = mean(day4_1),
  day4_2 = mean(day4_2),
  day4_3 = mean(day4_3)
)  

y1 <- y %>% pivot_longer(cols = starts_with("day"), names_to = "time", values_to = "score")

ggplot(data = y1, aes(x = as.factor(time), y = score, color = sex)) +
  geom_point() +
  geom_line(data = y1, aes(group = sex))

y2 <- y %>% group_by(sex) %>% summarise(
  day1 = day1_1 + day1_2 + day1_3,
  day2 = day2_1 + day2_2 + day2_3,
  day3 = day3_1 + day3_2 + day3_3,
  day4 = day4_1 + day4_2 + day4_3
)
y2$day3
y2 <- y2 %>% pivot_longer(cols = starts_with("day"), names_to = "time", values_to = "score")

ggplot(data = y2, aes(x = as.factor(time), y = score, color = sex)) +
  geom_point() +
  geom_line(data = y2, aes(group = sex))

### BEC --------------------------------------
h <- read_excel(f, sheet = 8, col_names = TRUE)
h <- h[2:7]
h[ h == "Sac" ] <- NA
h[ h == "N/A" ] <- NA
h <- na.omit(h)
h[4:6] <- sapply(h[4:6],as.numeric)

h <- h %>% mutate(
  average = rowMeans(h[, 4:6])
)

x <- h %>% group_by(Sex) %>% summarise(
  BEC = mean(average),
  SD = sd(average),
  n = length(average),
  se = SD/sqrt(n)
)

ggplot(data = x, aes(x = Sex, y = BEC, fill = c("red", "blue"))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = BEC - se, ymax = BEC + se), width = .5)
