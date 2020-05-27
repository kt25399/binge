f <- "SRTX2_SubjectData.xlsx"
  
  
binge <- function(f) {
  
  ### reading in intox score from file and wrangling----------------------------------------- 
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
  
  ### separating out per time point
  new <- e %>% pivot_longer(cols = starts_with("day"), names_to = "time", values_to = "score")
  newnew <- new %>% mutate(
    day = gsub("[[:punct:]]\\d", "", new$time),
    timepoint = gsub("\\D\\D\\D\\d[[:punct:]]", "", new$time)
  )
  
  ### Summarizing data
  y <- new %>% group_by(sex, time) %>% summarise(
    mean = mean(score),
    sd = sd(score),
    se = sd/sqrt(n())
  )
  
  ### Visualizing
  p1 <- ggplot(data = y, aes(x = as.factor(time), y = mean, color = sex)) +
    geom_point(size = 2) +
    geom_line(data = y, aes(group = sex)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
    ggtitle("Intox. per time point") +
    theme_classic() +
    labs(x = "Day", y = "Intox. Score") +
    scale_x_discrete(labels = c("", "day 1", "", 
                                "", "day2", "",
                                "", "day3", "",
                                "", "day4", "")) +
    scale_color_manual(values = c("#1aa700", "#2294d0"))
  
  ### Summarizing for intox per day
  z <- newnew %>% group_by(sex, day) %>% summarise(
    mean = mean(score),
    sd = sd(score),
    se = sd/sqrt(n())
  )
  
  ### Visualizing 
  p2 <- ggplot(data = z, aes(x = as.factor(day), y = mean, color = sex)) +
    geom_point(size = 2) +
    geom_line(data = z, aes(group = sex)) + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
    ggtitle("Intox. per day") +
    theme_classic() +
    labs(x = "Day", y = "Intox. Score") +
    scale_color_manual(values = c("#1aa700", "#2294d0"))
  
  ### reading in dose from file and wrangling----------------------------------------- 
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
  
  ### separating out per time point
  new <- g %>% pivot_longer(cols = starts_with("day"), names_to = "time", values_to = "dose")
  newnew <- new %>% mutate(
    day = gsub("[[:punct:]]\\d", "", new$time),
    timepoint = gsub("\\D\\D\\D\\d[[:punct:]]", "", new$time)
  )
  
  ### Summarizing data
  y <- new %>% group_by(sex, time) %>% summarise(
    mean = mean(dose),
    sd = sd(dose),
    se = sd/sqrt(n())
  )
  
  ### Visualizing
  p3 <- ggplot(data = y, aes(x = as.factor(time), y = mean, color = sex)) +
    geom_point(size = 2) +
    geom_line(data = y, aes(group = sex)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
    ggtitle("Dose per time point") +
    theme_classic() +
    labs(x = "Day", y = "Dose (mg/kg)") +
    scale_x_discrete(labels = c("", "day 1", "", 
                                "", "day2", "",
                                "", "day3", "",
                                "", "day4", "")) +
    scale_color_manual(values = c("#1aa700", "#2294d0"))
  
  ### Summarizing for intox per day
  z <- newnew %>% group_by(sex, day) %>% summarise(
    sum = sum(dose),
    sd = sd(dose),
    se = sd/sqrt(n())
  )
  
  ### Visualizing 
  p4 <- ggplot(data = z, aes(x = as.factor(day), y = sum, color = sex)) +
    geom_point(size = 2) +
    geom_line(data = z, aes(group = sex)) + 
    geom_errorbar(aes(ymin = sum - se, ymax = sum + se), width = 0.2) +
    ggtitle("Dose per day") +
    theme_classic() +
    labs(x = "Day", y = "Dose (mg/kg)") +
    scale_color_manual(values = c("#1aa700", "#2294d0"))
  
  ### reading in dose from file and wrangling----------------------------------------- 
  h <- read_excel(f, sheet = 8, col_names = TRUE)
  h <- h[2:7]
  h[ h == "Sac" ] <- NA
  h[ h == "N/A" ] <- NA
  h <- na.omit(h)
  h[4:6] <- sapply(h[4:6],as.numeric)
  
  ### Averaging BEC
  h <- h %>% mutate(
    average = rowMeans(h[, 4:6])
  )
  
  ### Summarizing
  x <- h %>% group_by(Sex) %>% summarise(
    BEC = mean(average),
    SD = sd(average),
    n = length(average),
    se = SD/sqrt(n)
  )
  
  ### Visualizing
  p5 <- ggplot(data = x, aes(x = Sex, y = BEC, fill = Sex)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = BEC - se, ymax = BEC + se), width = .5) +
    ggtitle("BEC") +
    theme_classic() +
    labs(x = "", y = "BEC (mg/dl)") +
    scale_fill_manual(values = c("#1aa700", "#2294d0"))
  
  ### reading in withdrawal from file and wrangling----------------------------------------- 
  j <- read_excel(f, sheet = 7, col_names = TRUE)
  j <- j[3:23, 1:20]
  j[ j == "Sac" ] <- NA
  j[ j == "N/A" ] <- NA
  j <- na.omit(j)
  j[4:20] <- sapply(j[4:20],as.numeric)
  
  names(j) <- c("subject", "group", "sex", "17:00", "18:00",
                "19:00", "20:00", "21:00", "22:00", "23:00",
                "24:00", "1:00", "2:00", "3:00", "4:00",
                "5:00", "6:00",  "7:00", "8:00",  "9:00")
  
  ### separating out per time point
  new <- j %>% pivot_longer(cols = 4:20, names_to = "time", values_to = "withdrawal")
  
  ### Summarizing data
  y <- new %>% group_by(sex, time) %>% summarise(
    mean = mean(withdrawal),
    sd = sd(withdrawal),
    se = sd/sqrt(n())
  )
  
  ### Visualizing
  p6 <- ggplot(data = y, aes(x = as.factor(time), y = mean, color = sex)) +
    geom_point(size = 2) +
    geom_line(data = y, aes(group = sex)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
    ggtitle("Average withdrawal per timepoint") +
    theme_classic() +
    labs(x = "Day", y = "Withdrawal") +
    scale_color_manual(values = c("#1aa700", "#2294d0"))
  
  ### Summarizing data
  y <- new %>% group_by(sex, time) %>% summarise(
    mean = max(withdrawal),
    sd = sd(withdrawal),
    se = sd/sqrt(n())
  )
  
  ### Visualizing
  p7 <- ggplot(data = y, aes(x = as.factor(time), y = mean, color = sex)) +
    geom_point(size = 2) +
    geom_line(data = y, aes(group = sex)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
    ggtitle("Max withdrawal per timepoint") +
    theme_classic() +
    labs(x = "Day", y = "Withdrawal") +
    scale_color_manual(values = c("#1aa700", "#2294d0"))
  
  ### Combining plots
  t <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, nrow = 3)
  return = list(c(p1, p2, p3, p4, p5, p6, p7, t))
}

binge(f = f)
