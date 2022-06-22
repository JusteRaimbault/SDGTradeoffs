

setwd(paste0(Sys.getenv('CS_HOME'),'/SDGTradeoffs/Models/MacroMultiModeling'))

library(ggplot2)
library(dplyr)
library(GGally)
library(readr)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

objectives <- c("gravityFlows", "oppAverageUtility", "averageDistance", "giniEconomicWealth", "oppWealth")

resprefix = 'OPTIMISATION_LOCAL_20220622_091715_res/';gen = '38000'#'5000'
#resprefix = 'OPTIMISATION_GRID_20220622_092133_res/';gen = '200'
resdir = paste0(Sys.getenv('CS_HOME'),'/SDGTradeoffs/Results/MacroMultiModeling/',resprefix,'/');dir.create(resdir,recursive = T, showWarnings = F)

res <- read_csv(paste0('openmole/optimisation/',resprefix,'population',gen,'.csv'))

res$innovationGravityDecay = cut(res$innovationGravityDecay,breaks = quantile(res$innovationGravityDecay,probs = c(0.2,0.4,0.6,0.8)))
#res$coevolGravityDecay = cut(res$coevolGravityDecay,3)
res$giniF = cut(res$giniEconomicWealth,breaks = quantile(res$giniEconomicWealth,probs = c(0.0,0.25,0.5,0.75,1.0)), include.lowest = T)
res$utilityF = cut(res$oppAverageUtility,breaks = quantile(res$oppAverageUtility,probs = c(0.0,0.25,0.5,0.75,1.0)), include.lowest = T)


ggsave(plot = ggpairs(res,aes(color=innovationGravityDecay),columns = objectives,
                      lower = list(continuous = wrap("points", alpha = 0.8,size=1)),
                      diag = list(continuous = wrap("densityDiag", alpha = 0.4))
)+stdtheme,filename = paste0(resdir,'scatter_models_colorinnovationGravityDecay.png'),width = 40,height=30,units='cm')

ggsave(plot = ggpairs(res,aes(color=giniF#, shape = utilityF
                              ),columns = objectives,
                      lower = list(continuous = wrap("points", alpha = 0.8,size=1)),
                      diag = list(continuous = wrap("densityDiag", alpha = 0.4))
)+stdtheme,filename = paste0(resdir,'scatter_models_colorginiEconomicWealth.png'),width = 40,height=30,units='cm')


