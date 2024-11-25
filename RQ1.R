
library(boot)
library(extrafont)
library(ggplot2)
library(tidyverse)
library(lme4)

load('Data-Full.RData')

FE.0 <- glmer(Value ~ logMagRatio + logDelayRatio - 1 +
                (logMagRatio + logDelayRatio - 1 | Subject),
              data = imported_data,
              family = binomial)

summary(FE.0)

magRecodeFrame <- data.frame(
  Magnitude = unique(imported_data$logMagRatio),
  Labels = c("LLR = 1/4\nof Current Risk",
             "LLR = 1/2\nof Current Risk",
             "LLR = SSR\n",
             "LLR = 2/1\nof Current Risk",
             "LLR = 4/1\nof Current Risk")
)

imported_data_rq1 <- imported_data
imported_data_rq1$predi <- predict(FE.0) %>% inv.logit()
imported_data_rq1$predg <- predict(FE.0, re.form = NA)
imported_data_rq1 <- imported_data_rq1 %>%
  mutate(logMagRatio = factor(logMagRatio,
                              levels = magRecodeFrame$Magnitude,
                              labels = magRecodeFrame$Labels))

ann_1 <- data.frame(LLRdelay = c(11.5, 11.5),
                    predi = c(0, 0.5),
                    Subject = rep(1, 2),
                    logMagRatio = factor(rep(magRecodeFrame[1,]$Magnitude, 2),
                                            levels = magRecodeFrame$Magnitude,
                                            labels = magRecodeFrame$Labels))

ann_2 <- data.frame(LLRdelay = c(2.25, 2.25),
                    predi = c(0, 0.5),
                    Subject = rep(1, 2),
                    logMagRatio = factor(rep(magRecodeFrame[2,]$Magnitude, 2),
                                         levels = magRecodeFrame$Magnitude,
                                         labels = magRecodeFrame$Labels))

ann_3 <- data.frame(LLRdelay = rep(115, 1),
                    predi = rep(0.525, 1),
                    Subject = rep(1, 1),
                    logMagRatio = factor(rep(magRecodeFrame[5,]$Magnitude, 1),
                                         levels = magRecodeFrame$Magnitude,
                                         labels = magRecodeFrame$Labels))

p1 <- ggplot(imported_data_rq1, aes(LLRdelay, predi,
                              #color = logMagRatio,
                              group = interaction(Subject, logMagRatio))) +
  geom_hline(yintercept = 0.5,
             linewidth = 0.5) +
  geom_line(data = ann_1, aes(LLRdelay, predi),
            color = 'black',
            linewidth = 0.5) +
  geom_line(data = ann_2, aes(LLRdelay, predi),
            color = 'black',
            linewidth = 0.5) +
  geom_text(data = ann_3,
    cex = 3,
    adj = 1,
    color = "black",
    family = "Times New Roman",
    label = "Level of Indifference",
    vjust = 'bottom',
  ) +
  geom_text(data = data.frame(
    LLRdelay = c(30, 20),
    predi = c(0.6, 0.55),
    Subject = rep(1, 2),
    Text = c("11.5 Days\nto 50%", "2.25 Days to 50%"),
    logMagRatio = factor(unique(imported_data$logMagRatio)[1:2],
                         levels = magRecodeFrame$Magnitude,
                         labels = magRecodeFrame$Labels)
  ),
  mapping = aes(
    LLRdelay, predi,
    group = interaction(Subject, logMagRatio),
    label = Text
  ),
  cex = 3,
  adj = 0.5,
  vjust = 'top',
  family = "Times New Roman",
  color = "black"
  ) +
  geom_line(alpha = 0.05) +
  geom_line(aes(LLRdelay, inv.logit(predg)),
            lty = 2,
            linewidth = 1) +
  facet_wrap( ~ logMagRatio,
              nrow = 1,
              ncol = 5) +
  xlab("Duration of Voluntary Removal") +
  ylab("Predicted Likelihood of Voluntary Removal") +
  labs(color = "Magnitude Difference",
       text = NA) +
  scale_x_log10() +
  theme_light() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Times New Roman",
                        color = 'black',
                        size = 12),
    strip.background = element_blank(),
    strip.text = element_text(color = 'black'),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.title.y = element_text(
      margin = margin(t = 0, r = 5, b = 0, l = 0)
    ),
    axis.title.x = element_text(
      margin = margin(t = 5, r = 0, b = 0, l = 0)
    )
  )

print(p1)

ggsave('figs/Figure 1.png',
       plot = p1,
       width = 8,
       height = 6,
       dpi = 300,
       device = 'png')
