
setwd(paste0(Sys.getenv('CS_HOME'),'/SDGTradeoffs/Models/FutureTrajectories'))

library(ggplot2)
library(dplyr)
library(readr)
library(reshape2)
library(GGally)
library(scales)

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



# PSE results


resprefix = 'PSE_EU_LOCAL_20250912_090708'
resdir = paste0(Sys.getenv('CS_HOME'),'/SDGTradeoffs/Results/FutureTrajectories/',resprefix,'/');dir.create(resdir,recursive = T, showWarnings = F)

res <- read_csv(paste0('pse/',resprefix,'.csv'),name_repair='minimal')

# populations
res_pop = res[,24:29]
names(res_pop) = c("avgLogPop1","avgLogPop2","avgLogPop3","stdLogPop1","stdLogPop2","stdLogPop3")
res_pop$id_traj=1:nrow(res_pop)
avg = melt(res_pop[, c("id_traj","avgLogPop1","avgLogPop2","avgLogPop3")],id.vars = c('id_traj'))
std = melt(res_pop[, c("id_traj","stdLogPop1","stdLogPop2","stdLogPop3")],id.vars = c('id_traj'))
avg$ymax = avg$value+std$value;avg$ymin = avg$value-std$value
ggsave(
  ggplot(avg,aes(x=variable,y=value,group=id_traj))+geom_point()+geom_line(color="#D3D3D3", alpha=0.5)#+
    #geom_errorbar(aes(x=variable,ymin=ymin,ymax=ymax))
  ,filename = paste0(resdir,'population_trajectories.png'),width = 30,height=30,units='cm'
)
  
# SDGs

sdg_indics = c("gravityFlows", "oppAverageUtility", "averageDistance", "giniEconomicWealth", "oppWealth")
# parse and average 
for(indic in sdg_indics){
  res[,indic]=sapply(unlist(res[,indic]),function(s){mean(as.numeric(strsplit(gsub("]","",gsub(pattern = "[","",s,fixed=T),fixed=T),split = ",",fixed = T)[[1]]))})
}

# scatterplot
#res$innovationGravityDecay = cut(res$innovationGravityDecay,breaks = quantile(res$innovationGravityDecay,probs = c(0.2,0.4,0.6,0.8)))
#ggsave(plot = ggpairs(res[,sdg_indics],columns = sdg_indics,
#                      lower = list(continuous = wrap("points", alpha = 0.8,size=1)),
#                      diag = list(continuous = wrap("densityDiag", alpha = 0.4))
#)+stdtheme,filename = paste0(resdir,'scatter-sdgs_colorinnovationGravityDecay.png'),width = 40,height=30,units='cm')

ggsave(
  ggplot(res[,sdg_indics],aes(x=gravityFlows,y=oppAverageUtility,color=averageDistance))+geom_point()+scale_color_continuous()
  ,filename = paste0(resdir,'sdgs_gravityFlows-oppAverageUtility_color-giniEconomicWealth.png'),width = 30,height=30,units='cm'
)

# radar plot

res_resc = res[sdg_indics]
res_resc$gravityFlows=log(res_resc$gravityFlows)
res_resc$color=rep("#D3D3D3",nrow(res_resc))
#res_resc$alpha=rep(0.15,nrow(res_resc))
objcols=hue_pal()(length(sdg_indics))
for(i in 1:length(objcols)){obj=sdg_indics[i];currentcol=objcols[i];
rowmin = res_resc[,obj]==min(res_resc[,obj])
res_resc$color[rowmin]=currentcol
#res_resc$alpha[rowmin]=1
}
for(obj in sdg_indics){res_resc[,obj] = (res_resc[,obj] - min(res_resc[,obj]))/(max(res_resc[,obj])-min(res_resc[,obj]))+0.1}
#sres = melt(data.frame(res_resc[,c(sdg_indics,"alpha","color")],id=1:nrow(res_resc)), measure.vars = sdg_indics, id.vars = c("id","color","alpha"))
sres = melt(data.frame(res_resc[,c(sdg_indics,"color")],id=1:nrow(res_resc)), measure.vars = sdg_indics, id.vars = c("id","color"))

indics_labels = c("Emissions","Innovation","Infrastructure","Inequalities","Wealth")
indics_nums = 1:length(sdg_indics);names(indics_nums)=sdg_indics
sres$variable=as.character(indics_nums[sres$variable])
g=ggplot(sres)
ggsave(
  g+geom_polygon(aes(x=variable,y=value,group=id,color=color), fill = NA)+ylim(c(0.0,1.1))+
    scale_color_identity()+scale_alpha_identity()+scale_size_identity()+ scale_x_discrete(labels=indics_labels)+
    coord_polar()+stdtheme+theme(axis.title=element_blank(),axis.ticks.y = element_blank(),axis.text.y=element_blank())
  ,filename = paste0(resdir,'radar_sdgs.png'),width = 30,height=30,units='cm'
)

