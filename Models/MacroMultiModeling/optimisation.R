

setwd(paste0(Sys.getenv('CS_HOME'),'/SDGTradeoffs/Models/MacroMultiModeling'))

library(ggplot2)
library(dplyr)
library(GGally)
library(readr)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

objectives <- c("gravityFlows", "oppAverageUtility", "averageDistance", "giniEconomicWealth", "oppWealth")

#resprefix = 'OPTIMISATION_LOCAL_20220622_091715_res/';gen = '38000'#'5000'
#resprefix = 'OPTIMISATION_GRID_20220622_092133_res/';gen = '200'
resprefix = 'OPTIMISATION_GRID_20220622_092133_res/';gen = '16800'
resdir = paste0(Sys.getenv('CS_HOME'),'/SDGTradeoffs/Results/MacroMultiModeling/',resprefix,'/');dir.create(resdir,recursive = T, showWarnings = F)

res <- read_csv(paste0('openmole/optimisation/',resprefix,'population',gen,'.csv'))

summary(res[,objectives])

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




# Polar plot
library(reshape2)
#library(ggiraphExtra)
library(scales)

# rescale indics with several orders of magnitude
res_resc = res
res_resc$gravityFlows=log(res_resc$gravityFlows)

# color traj min for each obj
res_resc$color=rep("#D3D3D3",nrow(res_resc))
res_resc$alpha=rep(0.15,nrow(res_resc))
objcols=hue_pal()(length(objectives))
for(i in 1:length(objcols)){obj=objectives[i];currentcol=objcols[i];
rowmin = res_resc[,obj]==min(res_resc[,obj])
res_resc$color[rowmin]=currentcol;res_resc$alpha[rowmin]=1
}

# need to reshape
for(obj in objectives){res_resc[,obj] = (res_resc[,obj] - min(res_resc[,obj]))/(max(res_resc[,obj])-min(res_resc[,obj]))+0.1}
sres = melt(data.frame(res_resc[,c(objectives,"alpha","color")],id=1:nrow(res_resc)), measure.vars = objectives, id.vars = c("id","color","alpha"))

#indics_labels = c("oppWealth"="Wealth","gravityFlows"="Emissions","giniEconomicWealth"="Inequalities","averageDistance"="Infrastructure","oppAverageUtility"="Innovation")
indics_labels = c("Emissions","Innovation","Infrastructure","Inequalities","Wealth")
indics_nums = 1:length(objectives);names(indics_nums)=objectives
#sres$variable=as.character(indics_labels[sres$variable])
sres$variable=as.character(indics_nums[sres$variable])

g=ggplot(sres)
#g+geom_line(aes(x=variable,y=value,group=id))+coord_radar()+scale_y_continuous()
ggsave(
g+geom_polygon(aes(x=variable,y=value,group=id,alpha=alpha,size=alpha,color=color), fill = NA)+ylim(c(0.0,1.1))+
  scale_color_identity()+scale_alpha_identity()+scale_size_identity()+ scale_x_discrete(labels=indics_labels)+
  coord_polar()+stdtheme+theme(axis.title=element_blank(),axis.ticks.y = element_blank(),axis.text.y=element_blank())
,filename = paste0(resdir,'radar_sdgs.png'),width = 30,height=30,units='cm'
)


