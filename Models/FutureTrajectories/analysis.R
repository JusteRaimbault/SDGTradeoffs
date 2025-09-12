
setwd(paste0(Sys.getenv('CS_HOME'),'/SDGTradeoffs/Models/FutureTrajectories'))

library(ggplot2)
library(dplyr)
library(readr)
library(reshape2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

resprefix = 'CALIBRATION_CN_LOCAL_20250911_204557'
#resprefix='CALIBRATION_EU_LOCAL_20250911_182514'
resdir = paste0(Sys.getenv('CS_HOME'),'/SDGTradeoffs/Results/FutureTrajectories/',resprefix,'/');dir.create(resdir,recursive = T, showWarnings = F)

res <- read_csv(paste0('calibration/',resprefix,'.csv'),name_repair='minimal')
res = res[,3:25]
names(res)[22:23]=c('logmse','mselog')

ggsave(
  ggplot(res,aes(x=logmse,y=mselog,color=innovationInnovationDecay))+geom_point()+scale_color_continuous(name='dI')+stdtheme,
  file=paste0(resdir,'pareto_color-innovationInnovationDecay.png'),width=25,height=20,units='cm'
)

res$mainSubmodel = apply(res[,c("innovationWeight","ecoWeight","coevolWeight")],1,function(row){
  if(max(row)==row[1]){'innovation'}
  if(max(row)==row[2]){'economy'}
  if(max(row)==row[3]){'infrastructure'}
})

ggsave(
  ggplot(res,aes(x=logmse,y=mselog,color=mainSubmodel))+geom_point()+scale_color_discrete(name='dI')+stdtheme,
  file=paste0(resdir,'pareto_color-mainSubmodel.png'),width=25,height=20,units='cm'
)


# Parameters distribution

table(res$innovationUtilityDistrib)
# -> 50%~ - can be removed (simpler for the plot)

params = res[,c(-8,-22,-23)]
# renormalise each param for visu (to check distrib shapes)
for(j in 1:ncol(params)){params[,j] = (unlist(params[,j]) - mean(unlist(params[,j])))/sd(unlist(params[,j]))}
d = melt(params,measure.vars = names(params))

ggsave(
  ggplot(d,aes(variable,value))+geom_violin()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)),
  file=paste0(resdir,'params-standardised-distribs.png'),width=40,height=20,units='cm'
)

# export boundaries at 1 IQ? or use quantiles ! -> 10-90% = 80% of params, good already!
# directly in correct format for PSE script
params = res[,c(-8,-22,-23)]
# ! check saturated bounds

#apply(params,MARGIN = 2,function(col){show(' in (',quantile(col,0.1),',',quantile(col,0.9),')')})  
for(j in 1:ncol(params)){
  writeLines(noquote((paste0(names(params)[j],' in (',quantile(unlist(params[,j]),0.1),',',quantile(unlist(params[,j]),0.9),'),'))))
}

