setwd("~/PremPlayerIndex")

players = read.csv("./Data/PremPlayerStats.csv")

library(tidyverse)

players = players %>%
  mutate(PrgP90 = PrgP/X90s,
         PrgC90 = PrgC/X90s,
         G.xG = Gls - xG,
         GxG90 = G.xG/X90s,
         Cmp90 = Cmp/X90s,
         Att90 = Att/X90s,
         Blk90 = Sh.1/X90s,
         Int90 = Int/X90s,
         INDEX = PrgP90*0.0054+PrgC90*0.0051+GxG90*0.2104+Cmp90*0.0020-Att90*0.0022+Int90*0.0084-0.0014*Mis)

players = players %>%
  arrange(desc(INDEX))

filtergames = players[players$X90s>=3,]
