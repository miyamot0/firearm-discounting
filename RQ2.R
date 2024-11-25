
library(boot)
library(extrafont)
library(ggplot2)
library(tidyverse)
library(lme4)

load('Data-Full.RData')

RQ2_FE.2 <- glmer(Value ~ logMagRatio + logDelayRatio - 1 +
                    logMagRatio:LastSuicidalThought + logDelayRatio:LastSuicidalThought +
                                                      logDelayRatio:AnySecure +
                                                      logDelayRatio:IntoleraceUncertainty.c +
                    (logMagRatio + logDelayRatio - 1 | Subject),
                  data = imported_data,
                  family = binomial)

summary(RQ2_FE.2)

magRecodeFrame <- data.frame(
  Magnitude = unique(imported_data$logMagRatio),
  Labels = c("LLR = 1/4\nof Current Risk",
             "LLR = 1/2\nof Current Risk",
             "LLR = SSR\n",
             "LLR = 2/1\nof Current Risk",
             "LLR = 4/1\nof Current Risk")
)

newdata <- expand.grid(
  logDelayRatio = seq(0.6931472, 4.7957905, length.out = 300),
  logMagRatio = c(-1.38629436111989, -0.6931472, 0),
  LastSuicidalThought = unique(imported_data$LastSuicidalThought),
  AnySecure = unique(imported_data$AnySecure),
  IntoleraceUncertainty.c = c(-1, 0, 1)
)

newdata$pred_g <- predict(RQ2_FE.2, newdata, re.form = NA)

newdata <- newdata %>%
  mutate(LastSuicidalThought = fct_relevel(LastSuicidalThought,
                                           "No SI",
                                           "More than one year ago",
                                           "Within the past year",
                                           "Within the past month"),
         IntoleraceUncertainty.c = factor(IntoleraceUncertainty.c,
                                          levels = c(-1, 0, 1),
                                          labels = c("-1 SD", "Mean", "+1 SD"))) %>%
  mutate(LLRDelay = (0 + 1)*exp(logDelayRatio) - 1) %>%
  mutate(logMagRatio = factor(logMagRatio,
                              levels = c(-1.38629436111989, -0.6931472, 0),
                              labels = c("LLR = 1/4 of Current Risk",
                                         "LLR = 1/2 of Current Risk",
                                         "LLR = Current Risk")))

p2 <- ggplot(newdata, aes(LLRDelay, inv.logit(pred_g),
                         lty = IntoleraceUncertainty.c,
                         color = logMagRatio,
                         group = interaction(logMagRatio, IntoleraceUncertainty.c))) +
  geom_hline(yintercept = 0.5,
             linewidth = 0.5) +
  geom_line(alpha = .5, linewidth = 0.75) +
  facet_grid(AnySecure ~ LastSuicidalThought) +
  xlab("Duration of Voluntary Removal") +
  ylab("Predicted Likelihood of Voluntary Removal") +
  labs(color = "Magnitude of Risk",
       lty = "Intolerance of Uncertainty") +
  scale_x_log10() +
  scale_colour_manual(
    values = c(
      "LLR = 1/4 of Current Risk" = "#000000",
      "LLR = 1/2 of Current Risk" = "#7f797e",
      "LLR = Current Risk" = "#ccc2ca"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "-1 SD" = 3,
      "Mean" = 1,
      "+1 SD" = 4
      )
    ) +
  theme_light() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Times New Roman",
                        color = 'black',
                        size = 12),
    strip.background = element_blank(),
    strip.text = element_text(color = 'black'),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA)
  ) +
  guides(
    colour = guide_legend(title.position = "top", title.hjust = 0.5),
    lty = guide_legend(title.position = "top", title.hjust = 0.5))

print(p2)

ggsave('figs/Figure 2.png',
       plot = p2,
       width = 8,
       height = 6,
       dpi = 300,
       device = 'png')
