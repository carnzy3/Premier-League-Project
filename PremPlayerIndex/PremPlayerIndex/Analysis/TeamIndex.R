setwd("~/PremPlayerIndex")

teams = read.csv("./Data/PremTeamStats.csv")

library(tidyverse)

teams = teams %>%
  mutate(WLD = substr(Result,1,1),
         Win = ifelse(WLD=='W',1,0),
         Loss = ifelse(WLD=='L',1,0),
         Pts = ifelse(Win==1,3,ifelse(Loss==1,0,1)),
         Draw = ifelse(WLD=='D',1,0),
         Poss = Poss/100,
         WeighMis = Mis/Poss,
         WeighDis = Dis/Poss,
         WeighTkl = TklW*Poss,
         WeighInt = Int*Poss,
         WeighBlk = Sh.3*Poss*Poss,
         OppxG = xG-xGD,
         OppG.xG = GA-OppxG)

library(fixest)

win_probit = glm(Win~PrgP+PrgC+G.xG+Cmp+WeighMis+WeighDis+WeighTkl+WeighInt+Att+WeighBlk+OppG.xG, family = binomial(link = "probit"), data = teams)
teams$InPosPred = predict(win_probit, type = "response")

winLPM = feols(Win~PrgP+PrgC+G.xG+Cmp+WeighMis+WeighDis+WeighTkl+WeighInt+Att+OppG.xG, data = teams)
etable(winLPM)

loss_probit = glm(Loss~PrgP+PrgC+G.xG+Cmp+WeighMis+WeighDis+WeighTkl+WeighInt+Att+WeighBlk, family = binomial(link = "probit"), data = teams)
teams$InPosLoss = predict(loss_probit, type = "response")

draw_probit = glm(Draw~PrgP+PrgC+G.xG+Cmp+WeighMis+WeighDis+WeighTkl+WeighInt+Att+WeighBlk, family = binomial(link = "probit"), data = teams)
teams$InPosDraw = predict(draw_probit, type = "response")

library(ggplot2)

ggplot(teams, aes(x = OppG.xG, y = InPosPred)) +
  geom_smooth(method = "loess", se = FALSE) + # smoothed curve
  labs(
    title = "Predicted Probabilities from Probit Model",
    x = "X1",
    y = "Predicted Probability"
  ) +
  theme_minimal()

ggplot(teams, aes(x = WeighTkl, y = InPosLoss)) +
  geom_smooth(method = "loess", se = FALSE) + # smoothed curve
  labs(
    title = "Predicted Probabilities from Probit Model",
    x = "X1",
    y = "Predicted Probability"
  ) +
  theme_minimal()

ggplot(teams, aes(x = WeighTkl, y = InPosDraw)) +
  geom_smooth(method = "loess", se = FALSE) + # smoothed curve
  labs(
    title = "Predicted Probabilities from Probit Model",
    x = "xVAR",
    y = "Predicted Probability"
  ) +
  theme_minimal()
