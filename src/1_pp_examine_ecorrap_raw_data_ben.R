#packages required
library(tidyverse)
library(brms)
library(lattice)
library(viridis)
library(tidybayes)
library(ggridges)
library(abind)
library(nlme)
library(ggplot2)

annastheme = theme_bw()+
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.key.width=unit(1.2,"cm"),
        axis.title=element_text(size=14),
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        strip.text = element_text(size = 14, colour = "black"),
        strip.background = element_rect("white"))


# Functions to transform between areas and diameters ####
to_area <- function(x) { #diameter to area
  pi*(x/2)^2
}
to_area_log <- function(x) {
  exp(pi*x^2)
}
to_diameter <- function(x) { #area to diameter
  2*sqrt((x) / pi)}
to_diameter_log <- function(x) { #logged area to diameter
  2*sqrt(exp(x) / pi)
}

setwd("C:/Users/bgrier/Documents/Projects/CoralDemography_EcoRRAP/1_AnnaIPMs/1_data")

ecorrap_dat = read.csv("EcoRRAP data for IPM_240226.csv")

ecorrap_dat = ecorrap_dat %>%
  dplyr::mutate(ln_colonyarea_t = log(size),
                ln_colonyarea_next_t = log(sizeNext)) %>%
  dplyr::filter(!is.na(size))  #we always need the starting timepoint to have a size


ecorrap_dat %>%
  group_by(dataset, Cluster) %>%
  summarise(count = n())

FT = "corym_non_acro"


FTlist = c("acro_table", "acro_corym", "corym_non_acro", "small_massive", "large_massive", "corym_non_acro_BR")
cols = c("#F8766D", "#A3A500", "green4", "royalblue3", "#E76BF3", "turquoise1")

#fecundity parameters from ReefMod
a_list= c(1.03, 1.69, -1.2, 0.86, 0.86, -1.2)
b_list= c(1.28, 1.05, 2.27,1.21, 1.21, 2.27)
fec_min_size = c(123, 134, 31, 38, 38, 31)


# Create an empty list to store plots
plot_list <- list()
plot_list_surv <- list()


#FT = 
plot_bins <- list()
data_processed = data.frame()
for (FT in FTlist) {
  print(FT)
  
  
  # Subset data for chosen FT
  lookup_taxa_match <- read.csv("output to summarise functional types in EcoRRAP data_240215_AC_KC.csv")
  taxa_inquestion <- subset(lookup_taxa_match, cscape_ft == FT)$Taxa
  dat <- subset(ecorrap_dat, Taxa %in% taxa_inquestion)
  #dat <- subset(dat, Cluster != "Inshore_Southern")
  

  
  max_diameter_IPM <- quantile(c(to_diameter(dat$size), to_diameter(dat$sizeNext)), probs = 0.99, na.rm = T)
  
  if (FT == "large_massive") U = log(to_area(200)) 
  if (FT %in% c( "small_massive","corym_non_acro", "corym_non_acro_BR")) U = log(to_area(40))
  if (FT == "acro_corym") U = log(to_area(50)) 
  if (FT == "acro_table") U = log(to_area(100)) 
  
  dat$change_abs = to_diameter(dat$sizeNext)-to_diameter(dat$size)
  
  dat_growth = subset(dat, to_use_for_growth == "yes" & !is.na(sizeNext) & !is.na(size) & !is.na(ubed90_median))
  summary(dat_growth)
  
  # dat_growth = subset(dat_growth, to_diameter(size) < max_diameter_IPM)
  
  # 
  # # Create the combined plot
  # p1 <- ggplot() +
  #   geom_histogram(data = dat_growth, aes(x = to_diameter(size), y = ..count.., fill = Taxa), color = "black" , alpha = 0.5) +  # Histogram
  #   geom_vline(xintercept = max_diameter_IPM, linetype = "dashed", colour = "red") +
  #   geom_vline(xintercept = to_diameter_log((U)), linetype = "dashed", colour = "black") +
  #   labs(title = "Density Plot with Histogram",
  #        x = "Diameter t0 (cm)",
  #        y = "Colony count") +  # Change the y-axis label
  #   ggtitle(FT) +
  #   theme_bw() +
  #   guides(fill = FALSE)  #+
    #geom_point(data = dat, aes(x = to_diameter(size), y = surv, fill = Taxa))
  

  #temporary to decide min size for IPMs
 # p1 = ggplot(dat, aes(x = to_diameter(size), fill = Taxa)) +
 #    geom_histogram(aes(y = ..count..), color = "black" , alpha = 0.5) +  # Histogram
 #    geom_vline(xintercept = max_diameter_IPM, linetype = "dashed", colour = "red") +
 #    geom_vline(xintercept = to_diameter_log((U)), linetype = "dashed", colour = "black") +
 #    labs(title = "Density Plot with Histogram",
 #         x = "Diameter t0 (cm)",
 #         y = "Colony count") +  # Change the y-axis label
 #    ggtitle(FT) +
 #    xlim(0,5) +
 #    theme_bw()
  
  # Save the plot
  #ggsave(paste("Initial size distribution", FT, ".png"), plot = p1, width = 7, height = 5)
  
  # Add the plot to the list
  # plot_list[[FT]] <- p1
  # 
  # 
  # p2 <- ggplot(dat_growth, aes(x = to_diameter(size), y = to_diameter(sizeNext), colour = Taxa)) +
  #   geom_point() +
  #   geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +  # Add this line
  #   labs(x = "Diameter t0 (cm)",
  #        y = "Diameter t1") +
  #   ggtitle(FT) +
  #   theme_bw()
  # 
  # 
  # 
  # # Add the plot to the list
  # plot_list[[paste(FT, "_next", sep = "")]] <- p2
  # 
  # #think this one is easier to interpret
  # p3 = ggplot(subset(dat_growth, to_diameter(size)<max_diameter_IPM), aes(x = to_diameter(size), y = change_abs, colour = Taxa)) +
  #   geom_point() +
  #   geom_hline(yintercept = 0,  color = "black", linetype = "dashed") +  # Add this line
  #   labs(x = "Diameter t0 (cm)",
  #        y = "Annual change in diameter (cm)") +
  #   ggtitle(FT) +
  #   theme_bw()
  # 
  # plot_list[[paste(FT, "_next", sep = "")]] <- p3
  
  #average change in diameter per size class
  # dat_growth = dat_growth %>%
  # mutate(size_bin = cut(to_diameter(size), breaks = c(0,2.5, 5,7.5,10,15,20,30,40,50,100,150), 
  #                       labels = c("0-2.5", "2.5-5","5-7.5", "7.5-10", "10-15","15-20", "20-30", "30-40", "40-50",
  #                                  "50-100", ">150"))) 
  # 
  # dat_growth_summary = dat_growth %>%
  #   group_by(size_bin) %>%
  #   summarise(mean_growth = mean(change_abs), 
  #             sd_surv = sd(change_abs),
  #             n = n())
  # 
  # p_growth_bin = ggplot(dat_growth_summary, aes(x = size_bin, y = mean_growth)) +
  #   geom_bar(stat = "identity", fill = cols[which(FTlist == FT)], colour = "black") +
  #   geom_errorbar(aes(ymin = mean_growth - sd_surv, ymax = mean_growth + sd_surv), 
  #                 width = 0.2, 
  #                 colour = "black") +
  #   geom_text(aes(label = n), vjust = -1, size = 3) +  # Adjust y to position the text correctly
  #   labs(x = "Diameter t0 (cm)",
  #        y = "Change in diameter (cm)") +
  #   ggtitle(paste(FT, "Mean growth rate")) +
  #   theme_bw() +
  #   coord_cartesian(ylim = c(-4, 8)) +
  #   scale_y_continuous(breaks = seq(-4, 8, by = 2))
  # 
  # plot_bins[[FT]] <- p_growth_bin
  
  
#survival in bins
  dat_surv = subset(dat, surv != "remove" & !is.na(size) & !is.na(ubed90_median) & !is.na(surv))
  dat_surv$surv = as.numeric(dat_surv$surv)
  # dat_surv = subset(dat_surv, to_diameter(size) <max_diameter_IPM)
  print(paste("ncolonies survival", nrow(dat_surv)))
  
  # dat_surv$size_bin = to_diameter(dat_surv$size) 
  #   dat_surv = dat_surv %>%
  # mutate(size_bin = cut(size_bin, breaks = c(0,2.5, 5,7.5,10,15,20,30,40,50,100,150), 
  #                        labels = c("0-2.5", "2.5-5","5-7.5", "7.5-10", "10-15","15-20", "20-30", "30-40", "40-50",
  #                                   "50-100", ">150"))) 
  #   
  #   dat_surv_summary = dat_surv %>%
  #     filter(dataset == "photogrammetry") %>%
  #     group_by(size_bin) %>%
  #     summarise(mean_survival = mean(surv), 
  #               sd_surv = sd(surv),
  #               n=n())
  # 
  # p_surv = ggplot(dat_surv_summary, aes(x = size_bin, y = mean_survival)) +
  #   geom_point(colour = cols[which(FTlist == FT)], size = 3) +
  #   # geom_errorbar(aes(ymin = mean_survival - sd_surv, ymax = mean_survival + sd_surv), width = 0.1, colour = cols[which(FTlist == FT)]) +  # Add error bars
  #   geom_text(aes(label = n), y = 0, vjust = -0.5, size = 3) +  # Add sample size text
  #   labs(x = "Diameter t0 (cm)",
  #        y = "Survival (1=alive)") +
  #   ggtitle(FT) +
  #   theme_bw() +
  #   ylim(0,1) +
  #   ggtitle(paste(FT, "photogrammetry only"))
  # 
  # dat_surv_summary2 = dat_surv %>%
  #   #filter(dataset == "photogrammetry") %>%
  #   group_by(size_bin) %>%
  #   summarise(mean_survival = mean(surv), 
  #             sd_surv = sd(surv),
  #             n=n())
  # 
  # p_surv2 = ggplot(dat_surv_summary2, aes(x = size_bin, y = mean_survival)) +
  #   geom_point(colour = cols[which(FTlist == FT)], size = 3) +
  #   # geom_errorbar(aes(ymin = mean_survival - sd_surv, ymax = mean_survival + sd_surv), width = 0.1, colour = cols[which(FTlist == FT)]) +  # Add error bars
  #   geom_text(aes(label = n), y = 0, vjust = -0.5, size = 3) +  # Add sample size text
  #   labs(x = "Diameter t0 (cm)",
  #        y = "Survival (1=alive)") +
  #   ggtitle(FT) +
  #   theme_bw() +
  #   ylim(0,1) +
  #   ggtitle(paste(FT, "Mean survival"))
  # 
  # plot_list_surv[[FT]] <- p_surv
  # plot_list_surv[[paste(FT, "_next", sep = "")]] <- p_surv2
  # plot_bins[[paste(FT, "_next", sep = "")]] <- p_surv2
  
  surv_growth_data = dplyr::left_join(dat_surv[,c('Taxa','Site_UID','ColonyID','size','surv')],
                                      dat_growth[,c('Taxa','Site_UID','ColonyID','size','change_abs')],
                                      by = c('Taxa','Site_UID', 'ColonyID', 'size'))
  surv_growth_data$group = FT
  surv_growth_data$size_diam = to_diameter(surv_growth_data$size)
  
  data_processed = rbind(data_processed, surv_growth_data)
}

# Now, plot the saved plots using cowplot
# cowplot::plot_grid(plotlist = plot_list, ncol = 2)
# 
# setwd("C:/Users/acresswe/OneDrive - Australian Institute of Marine Science/Documents/GitHub/CoralDemography_EcoRRAP/1_AnnaIPMs/1_plots")
# ggsave("Growth_cscape_groups_raw_data.png", width = 12, height = 20)
# 
# 
# cowplot::plot_grid(plotlist = plot_list_surv, ncol = 2)
# 
# setwd("C:/Users/acresswe/OneDrive - Australian Institute of Marine Science/Documents/GitHub/CoralDemography_EcoRRAP/1_AnnaIPMs/1_plots")
# ggsave("Survival_cscape_groups_raw_data.png", width = 12, height = 20)

# survival and growth in bins

# cowplot::plot_grid(plotlist = plot_bins, ncol = 2)
# 
# setwd("C:/Users/acresswe/OneDrive - Australian Institute of Marine Science/Documents/GitHub/CoralDemography_EcoRRAP/1_AnnaIPMs/1_plots")
# ggsave("Survival_cscape_groups_raw_data.png", width = 12, height = 20)
# 
# 
# surv_growth_data = dplyr::left_join(dat_surv[,c('Taxa','Site_UID','ColonyID','size','surv')],
#                             dat_growth[,c('Taxa','Site_UID','ColonyID','size','change_abs')],
#                             by = c('Taxa','Site_UID', 'ColonyID', 'size'))
# surv_growth_data$diam = to_diameter(surv_growth_data$size)

acro_table_test = data_processed[data_processed$group == "acro_table",]

mean_squared_error <-function(y_pred, y_actual){ 
  ss_residuals = sum((y_actual - y_pred)^2,na.rm=T)
  mse = ss_residuals / length(y_actual)
  return(mse)
}


#optimal binning just considering distribution of sizes 10 bins
test_optbin_10 = optbin::optbin(acro_table_test$size_diam, 10) # start with 6 bins
summary(test_optbin_10)
plot(test_optbin_10)

optbin_10_bins = acro_table_test %>% mutate(upper_size = cut(size_diam, breaks = c(0, test_optbin_10$thr),
                                                        labels = as.character(test_optbin_10$thr)))
optbin_10_bins = optbin_10_bins %>% group_by(upper_size) %>% mutate(pred_surv = mean(surv), pred_growth = mean(change_abs, na.rm=T))
optbin_10_mse = optbin_10_bins %>% group_by(upper_size) %>% summarise(mse_surv = mean_squared_error(pred_surv, surv),
                                                              mse_growth = mean_squared_error(pred_growth, change_abs))

#optimal binning just considering distribution of sizes 20 bins
test_optbin_20 = optbin::optbin(acro_table_test$size_diam, 20) # start with 6 bins
summary(test_optbin_20)
plot(test_optbin_20)

optbin_20_bins = acro_table_test %>% mutate(upper_size = cut(size_diam, breaks = c(0, test_optbin_20$thr),
                                                        labels = as.character(test_optbin_20$thr)))
optbin_20_bins = optbin_20_bins %>% group_by(upper_size) %>% mutate(pred_surv = mean(surv), pred_growth = mean(change_abs, na.rm=T))
optbin_20_mse = optbin_20_bins %>% group_by(upper_size) %>% summarise(mse_surv = mean_squared_error(pred_surv, surv),
                                                              mse_growth = mean_squared_error(pred_growth, change_abs))

test_optbin_60 = optbin::optbin(acro_table_test$size_diam, 60) # start with 6 bins
summary(test_optbin_60)
plot(test_optbin_60)

optbin_60_bins = acro_table_test %>% mutate(upper_size = cut(size_diam, breaks = c(0, test_optbin_60$thr),
                                                             labels = as.character(test_optbin_60$thr)))
optbin_60_bins = optbin_60_bins %>% group_by(upper_size) %>% mutate(pred_surv = mean(surv), pred_growth = mean(change_abs, na.rm=T))
optbin_60_mse = optbin_60_bins %>% group_by(upper_size) %>% summarise(mse_surv = mean_squared_error(pred_surv, surv),
                                                                      mse_growth = mean_squared_error(pred_growth, change_abs))

#optimal binning considering colony sizes and their binary survival 
test_varbin = varbin::varbin(acro_table_test[,c('size_diam','surv')], 'size_diam', 'surv')
varbin::varbin.plot(test_varbin)
varbin_bins = acro_table_test %>% mutate(upper_size = cut(size_diam, breaks = c(0, 1.65, 3.35, 7.481, 10.308, 14.484, 20.509,
                                                                                24.942, 29.7056, 46.901, max(acro_table_test$size_diam)),
                                                          labels = test_varbin$Cutpoint[1:(length(test_varbin$Cutpoint)-2)]))
varbin_bins = varbin_bins %>% group_by(upper_size) %>% mutate(pred_surv = mean(surv), pred_growth = mean(change_abs, na.rm=T))
varbin_mse = varbin_bins %>% group_by(upper_size) %>% summarise(mse_surv = mean_squared_error(pred_surv, surv),
                                                                mse_growth = mean_squared_error(pred_growth, change_abs))

#optimal binning considering colony sizes and their growth (growth is only recorder for corals that have survival 1)
test_RT_G = rpart::rpart(change_abs ~ size_diam, data = acro_table_test)
plot(test_RT_G)
text(test_RT_G)
summary(test_RT_G)

test_RT_S = rpart::rpart(surv ~ size_diam, data = acro_table_test)
plot(test_RT_S)
text(test_RT_S)
summary(test_RT_S)


#Testing manual binning values 
anna_bins = acro_table_test %>%
  mutate(size_bin = cut(size_diam, breaks = c(0,2.5, 5,7.5,10,15,20,30,40,50,100,150), 
                          labels = c("0-2.5", "2.5-5","5-7.5", "7.5-10", "10-15","15-20", "20-30", "30-40", "40-50",
                                     "50-100", ">150"))) 

anna_bins = anna_bins %>% group_by(size_bin) %>% mutate(pred_surv = mean(surv), pred_growth = mean(change_abs, na.rm=T))
ab_mse = anna_bins %>% group_by(size_bin) %>% summarise(mse_surv = mean_squared_error(pred_surv, surv),
                                                mse_growth= mean_squared_error(pred_growth, change_abs))

cust_bins = acro_table_test %>% mutate(upper_size = cut(size_diam, breaks = c(0,1.25,2.5,3.75,5,7.5,10,12.5,15,20,25,30,35,40,45,50,75,100,150),
                                                      labels = c("1.25","2.5","3.75","5","7.5","10","12.5","15","20","25","30","35","40","45",
                                                                 "50","75","100","150")))
cust_bins = cust_bins %>% group_by(upper_size) %>% mutate(pred_surv = mean(surv), pred_growth = mean(change_abs, na.rm=T))
cb_mse = cust_bins %>% group_by(upper_size) %>% summarise(mse_surv = mean_squared_error(pred_surv, surv),
                                                        mse_growth= mean_squared_error(pred_growth, change_abs))


adria_bins = acro_table_test %>% mutate(upper_size = cut(size_diam, breaks = c(0,1,2,6,15,36,90),
                                                         labels = c("0-1","1-2","2-6","6-15","15-36","36-90")))
adria_bins = adria_bins %>% group_by(upper_size) %>% mutate(pred_surv = mean(surv), pred_growth = mean(change_abs,na.rm=T))
adria_mse = adria_bins %>% group_by(upper_size) %>% summarise(mse_surv = mean_squared_error(pred_surv, surv),
                                                              mse_growth= mean_squared_error(pred_growth, change_abs))
