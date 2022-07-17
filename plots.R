
CHDAGE <- read.csv("data/CHDAGE.csv", header= TRUE)

#============================

mean(CHDAGE[,2])

#============================

library(tidyverse)
library(dplyr)

CHDAGE <- CHDAGE %>% 
  mutate(
    # Create categories
    Age_group = dplyr::case_when(
      AGE >= 20 & AGE < 30 ~ "20-29",
      AGE >= 30 & AGE < 35 ~ "30-34",
      AGE >= 35 & AGE < 40 ~ "35-39",
      AGE >= 40 & AGE < 45 ~ "40-44",
      AGE >= 45 & AGE < 50 ~ "45-49",
      AGE >= 50 & AGE < 55 ~ "50-54",
      AGE >= 55 & AGE < 60 ~ "55-59",
      AGE >= 60        ~ "60-69"
    ),
  # Convert to factor
  Age_group = factor(
    Age_group,
    level = c("20-29", "30-34","35-39", "40-44", "45-49", "50-54", "55-59", "60-69")
    )
  )
  

tb1 <- CHDAGE %>%
  group_by(Age_group, CHD) %>%
  summarise(no_cases = n()) %>% 
  pivot_wider(names_from = c(CHD), values_from = no_cases) %>%
  mutate(Total = `0`+`1`) %>% 
  mutate("Yes %" = round(`1`/ `Total` * 100, digits = 0)) 

DT::datatable(tb1)

#============================
tb1 %>% ggplot(aes(x = Age_group, y = tb1$"Yes %", fill = "red")) +
        geom_bar(stat="identity", color = "black") +
        theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5)) +
        labs (title = "Percentage of coronary disease in age groups", 
              x = "Age group", y = "Coronary Disease (mean)") +
        scale_fill_brewer(palette="Accent") + 
        theme(legend.position="none")

#============================

lgreg_plot <- glm(CHD ~  AGE, data = CHDAGE, binomial(logit))
summary(lgreg_plot)
range(CHDAGE$CHD)
range(CHDAGE$AGE)
x1re_ta <- seq(10, 80, 3)
yx1 <- predict(lgreg_plot, list(AGE = x1re_ta), type="response")


plot(CHD ~  AGE, data = CHDAGE, 
     col = "orangered", pch = "|", ylim = c(-0.2, 1.2), xlim = c(10, 80),
     main = "using logistic regression function for binomial Y ")
abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
lines(x1re_ta, yx1, lty = 1, lwd = 3, col = "navy")

#============================
lgreg_plot

summary(lgreg_plot)


lgreg_plot <- glm(Y ~  X1, data = bankrup, binomial(logit))
summary(lgreg_plot)
range(bankrup$Y)
range(bankrup$X1)
x1re_ta <- seq(-309, 69, 3)
yx1 <- predict(lgreg_plot, list(X1 = x1re_ta), type="response")



plot(Y ~  X1, data = bankrup, 
     col = "orangered", pch = "|", ylim = c(-0.2, 1.2),
     main = "using logistic regression function for binomial Y ")
abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
lines(x1re_ta, yx1, lty = 1, lwd = 3, col = "navy")

#============================

round(exp(-0.5503398 + 0.1573639*(-18.1) + 0.1947428*(-6.5))
      /
        (1+exp(-0.5503398 + 0.1573639*(-18.1) + 0.1947428*(-6.5))), 4)

round(1/(1+exp(-(-0.5503398 + 0.1573639*(-18.1) + 0.1947428*(-6.5)))), 4)
