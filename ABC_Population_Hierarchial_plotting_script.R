
#load the packages 
library( dplyr)
library( ggplot2)
HDIofMCMC             = function( sampleVec , credMass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = ceiling( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}
#########################################################################################################
setwd("/Users/michaelcollins/Documents/Thoughts/Aproximate Bayesian Computation")
#########################################################################################################
#Load the Population Parameters
Population_Parameters = read.table("Population_Parameters_PPE_Results.txt", header=T)
#----------------
#Subject Level Parameters
Subject_Parameters = read.table("Subject_Parameters_PPE_Results.txt", header=T)
#----------------
#Item Parameter Values
Item_Parameters = read.table("Individual_Parameters_PPE_Results.txt", header=T)
#----------------
#Performance Values
Performance_Parameters = read.table("Performance_PPE_Results.txt", header=T)
#########################################################################################################
#Posterior Distribution of Sample Distribution
ggplot(Population_Parameters[Population_Parameters$Sample > 1000,], aes("b", Population_b_mean_Post)) +
  geom_violin(fill = 1) + 
  geom_violin(aes("m"   , Population_m_mean_Post), fill =2 ) +
  geom_violin(aes("s"   , Population_s_mean_Post), fill = 3) +
  geom_violin(aes("tau" , Population_tau_mean_Post), fill = 4) +
  xlab("Parameters") + ylab("Parameter Values") +
  ggtitle("Sample Distribution Posterior Values") + 
  theme(axis.title = element_text(size = 15),
        axis.text  = element_text(size = 10),
        title      = element_text(size = 18))

ggplot(Population_Parameters[Population_Parameters$Sample > 1000,], aes("b", Population_b_sd_Post)) +
  geom_violin(fill = 1) + 
  geom_violin(aes("m"   , Population_m_sd_Post), fill =2 ) +
  geom_violin(aes("s"   , Population_s_sd_Post), fill = 3) +
  geom_violin(aes("tau" , Population_tau_sd_Post), fill = 4) +
  xlab("Parameters") + ylab("Parameter Values") +
  ggtitle("Sample Distribution Posterior Values") + 
  theme(axis.title = element_text(size = 15),
        axis.text  = element_text(size = 10),
        title      = element_text(size = 18))
#########################################################################################################
#Posterior Distribution of Subject Distribution
ggplot(Subject_Parameters[Subject_Parameters$Sample > 1000,], aes("b", Subject_b_mean_Post)) +
  geom_violin(fill = 1) + 
  geom_violin(aes("m"   , Subject_m_mean_Post), fill =2 ) +
  geom_violin(aes("s"   , Subject_s_mean_Post), fill = 3) +
  geom_violin(aes("tau" , Subject_tau_mean_Post), fill = 4) +
  xlab("Parameters") + ylab("Parameter Values") +
  ggtitle("Subject Distribution Posterior Values") + 
  theme(axis.title = element_text(size = 15),
        axis.text  = element_text(size = 10),
        title      = element_text(size = 18))

ggplot(Subject_Parameters[Subject_Parameters$Sample > 1000,], aes("b", Subject_b_sd_Post)) +
  geom_violin(fill = 1) + 
  geom_violin(aes("m"   , Subject_m_sd_Post), fill =2 ) +
  geom_violin(aes("s"   , Subject_s_sd_Post), fill = 3) +
  geom_violin(aes("tau" , Subject_tau_sd_Post), fill = 4) +
  xlab("Parameters") + ylab("Parameter Values") +
  ggtitle("Subject Distribution Posterior Values") + 
  theme(axis.title = element_text(size = 15),
        axis.text  = element_text(size = 10),
        title      = element_text(size = 18))
#########################################################################################################
#Item Distribution of Subject Distribution
colnames(Item_Parameters)
ggplot(Item_Parameters[Item_Parameters$Sample > 1000,], aes("b", Item_b_Post, group = paste(user_id, StimID) ) ) +
  geom_violin(fill = 1) + 
  geom_violin(aes("m"   , Item_m_Post), fill =2 ) +
  geom_violin(aes("s"   , Item_s_Post), fill = 3) +
  geom_violin(aes("tau" , Item_tau_Post), fill = 4) +
  xlab("Parameters") + ylab("Parameter Values") +
  ggtitle("Item Distribution Posterior Values") + 
  theme(axis.title = element_text(size = 15) ,
        axis.text  = element_text(size = 10) ,
        title      = element_text(size = 18) )
#########################################################################################################
#########################################################################################################
#########################################################################################################
#Plot the Performance
#########################################################################################################
LS = unique(
  Performance_Parameters[Performance_Parameters$Sample > 1000,] %>%
      group_by(Learning_Schedule, repetition) %>%
      mutate(DV = mean(performance),
             PPE_Post = mean(Perf_Post),
             H_HDI = HDIofMCMC(Perf_Post)[1],
             L_HDI = HDIofMCMC(Perf_Post)[2] ) %>%
   select(Learning_Schedule, repetition, DV, PPE_Post, H_HDI, L_HDI)
  )

User = unique(
  Performance_Parameters[Performance_Parameters$Sample > 1000,] %>%
    group_by(user_id, Learning_Schedule, repetition) %>%
    mutate(DV = mean(performance),
           PPE_Post = mean(Perf_Post),
           H_HDI = HDIofMCMC(Perf_Post)[1],
           L_HDI = HDIofMCMC(Perf_Post)[2] ) %>%
    select(user_id, Learning_Schedule, repetition, DV, PPE_Post, H_HDI, L_HDI)
)
################################################################################################################
ggplot(LS, aes(repetition, DV) ) +
  geom_line(aes(col = "Human") ) +
  geom_line(aes(repetition, PPE_Post, col = "PPE_Post") ) +
  geom_ribbon(aes(ymin = H_HDI, ymax = L_HDI, fill = "PPE_Post"), alpha = .25 ) +
  facet_wrap(~Learning_Schedule) +
  xlab("Trial") + ylab("Performance") + 
  theme(legend.title = element_blank())
#----------------
ggplot(User, aes(repetition, DV, group = user_id) ) +
  geom_line(aes(col = "Human") ) +
  geom_line(aes(repetition, PPE_Post, col = "PPE_Post"), lty = 2 ) +
  geom_ribbon(aes(ymin = H_HDI, ymax = L_HDI, fill = "PPE_Post"), alpha = .25 ) +
  facet_wrap(~Learning_Schedule) +
  xlab("Trial") + ylab("Performance") + 
  theme(legend.title = element_blank())


