setwd('c:/Users/bgrier/Documents/Projects/EcoRRAP Analysis/')
ecorrap_data = read.csv('20240522_All_Joined_cleanedbycode.csv')
library(ggplot2)

unique(ecorrap_data$ID_2022)
diameter <- function(x) { #area to diameter
  2 * sqrt((x) / pi)
  }

ecorrap_data$diam_2022 = diameter(ecorrap_data$Area_2022)*100
ecorrap_data$diam_2021 = diameter(ecorrap_data$Area_2021)*100

ecorrap_data$larger_than_5_2021 = ifelse(ecorrap_data$diam_2021 > 5, "greater_than_5", "smaller_than_5")
ecorrap_data$larger_than_5_2022 = ifelse(ecorrap_data$diam_2022 > 5, "greater_than_5", "smaller_than_5")
ecorrap_data$larger_than_1_2021 = ifelse(ecorrap_data$diam_2021 > 1, "greater_than_1", "smaller_than_1")
ecorrap_data$larger_than_3_2021 = ifelse(ecorrap_data$diam_2021 > 3, "greater_than_3", "smaller_than_3")


ggplot() + geom_jitter(data = ecorrap_data[ecorrap_data$larger_than_5_2021 == 'smaller_than_5',], 
                       aes(x = as.factor(surv), y = diam_2021))

ggplot() + geom_histogram(data = ecorrap_data, aes(surv, fill= larger_than_5_2021)) + 
          facet_wrap(~Reef)


table(ecorrap_data$surv, ecorrap_data$larger_than_5_2021)

221/(221+1132)

1266/(1266+13724)

1132/(221+1132)

13724/(1266+13724)

surv_prop = function(x){
  surv = length(x[x==1])
  total = length(x)
  return(surv/total)
}

test = ecorrap_data %>% group_by(Reef, larger_than_5_2021) %>%
  mutate(surv_p = surv_prop(surv))

props = unique(test[,c('Reef','larger_than_5_2021','surv_p')])

props_less_than1 = ecorrap_data %>% group_by(Reef, larger_than_1_2021) %>%
  mutate(surv_p = surv_prop(surv))
props_less_than1 = unique(props_less_than1[,c('Reef','larger_than_1_2021','surv_p')])            

test = read.csv('../CoralDemography_EcoRRAP/1_AnnaIPMs/1_data/EcoRRAP data for IPM_240226.csv')


