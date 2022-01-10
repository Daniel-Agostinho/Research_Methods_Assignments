# clean console, remove variables and close plots =============================

cat("\014")
rm(list=ls())
while (!is.null(dev.list())){
  dev.off()
}

options(device = "windows") # plots in pop up windows

# set directory
#setwd("C:\\Users\\danie\\Desktop\\PhD\\1ºSemestre\\Research_Methods")
setwd('C:\\Users\\Julia\\Desktop\\Research_Methods')

# libraries

library(ggplot2)
library(rstatix)
library(RColorBrewer)
library(dplyr)

# reading data
outputdata = read.table("dataset.txt", header = TRUE, dec = ".")

select_data_by_exam = function(data_set, exam_number){
  return(data_set[data_set$Exams == exam_number, ])
}

select_data_by_prob = function(data_set, prob_number){
  return(data_set[data_set$P_Collision == prob_number, ])
}

corrected_data = function(data_set){
  cpu_time = data_set$CPU_Time
  cpu_time[cpu_time > 300.0] = 300.0
  output = data.frame(data_set$Code_Type, data_set$Number_of_Exams, data_set$Collision_Prob, cpu_time)
  names(output) = c("Code", "Exams", "P_Collision", "CPU_Time")
  return(output)
}

data_normality_check = function(data_set){
  cpu_list = list(data_set$CPU_Time, data_set[data_set$Code == "code1",]$CPU_Time, data_set[data_set$Code == "code2",]$CPU_Time)
  plot_name = c("All data distribution", "Code 1 data distribution", "Code 2 data distribution")
  indx = 1
  x11()
  par(mfrow=c(1,3))
  for (variable in cpu_list) {
    histogram = hist(variable,   plot = FALSE)
    histogram$counts=histogram$counts/sum(histogram$counts)
    plot(histogram, xlab = "CPU Time", main = plot_name[indx], col = c(brewer.pal(n = 8, name = 'Pastel1')))
    indx = indx + 1
  }
}

behaviour_analysis = function(data_set){
  
  plot_data = data_set[data_set$Code == "code1", ]
  
  probs = c(0.1, 0.2, 0.3, 0.4, 0.5)
  linear = c(35,30,30,25,20)
  
  quad = list(seq(40, 75, by=5), seq(35, 60, by=5), seq(35, 55, by=5),
              seq(30, 45, by=5), seq(25, 40, by=5))
  
  
  x11()
  par(mfrow=c(1,5))
  
  for (i in c(1:length(probs))) {
    prob = probs[i]
    prob_data = select_data_by_prob(plot_data, prob)
    exam_data = prob_data[prob_data$Exams <= linear[i], ]
    boxplot(CPU_Time ~ Exams, data = exam_data, xlab = "Number of Exams", ylab = "CPU Time",
         main = sprintf("Probability of colision %.2f", prob), col = c(brewer.pal(n = 9, name = 'Pastel1')))

  }

  
  places = list(c(65, 150),c(55, 150),c(45, 150),c(40,120),c(35,100))
  indx = 1
  x11()
  par(mfrow=c(1,5))
  
  for (i in c(1:length(probs))) {
    prob = probs[i]
    exams = quad[[i]]
    mean_cpu = c()
    for (exam in exams){
      exam_data = select_data_by_exam(select_data_by_prob(plot_data, prob), exam)
      mean_cpu = c(mean_cpu, mean(exam_data$CPU_Time))
    }
    plot(exams, mean_cpu, xlab = "Number of Exams", ylab = "Mean CPU Time",
         main = sprintf("Probability of colision %.2f", prob), pch = 19)
    regress = lm(mean_cpu ~ exams)
    abline(regress)
    r_value = sqrt(summary(regress)$r.squared)
    x_pos = places[[indx]][1]
    y_pos = places[[indx]][2]
    text(x_pos, y_pos,  paste(paste(sprintf("r = %.4f", r_value))))
    indx = indx + 1
  }
  
  
}


# Main



outputdata = corrected_data(outputdata)
behaviour_analysis(outputdata)

data_normality_check(outputdata)

# 3way anova -> code, exams, probs
three_way_anova = aov(CPU_Time ~ Code*as.factor(Exams)*as.factor(P_Collision), data = outputdata)
summary(three_way_anova)
three_way_table = data.frame(unclass(summary(three_way_anova)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv2(three_way_table,"Three_way_data.csv", row.names = TRUE)


# 2way anova -> exams, probs
two_way_anova = aov(CPU_Time ~ as.factor(Exams)*as.factor(P_Collision), data = outputdata[outputdata$Code == 'code1', ])
summary(two_way_anova)
two_way_table = data.frame(unclass(summary(two_way_anova)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv2(two_way_table,"Two_way_data.csv", row.names = TRUE)
coefs = two_way_anova$coefficients
coef_exams = c(coefs["as.factor(Exams)15"], coefs["as.factor(Exams)20"], coefs["as.factor(Exams)25"],coefs["as.factor(Exams)30"],coefs["as.factor(Exams)35"],coefs["as.factor(Exams)40"],
               coefs["as.factor(Exams)45"],coefs["as.factor(Exams)50"],coefs["as.factor(Exams)55"],coefs["as.factor(Exams)60"], coefs["as.factor(Exams)65"], coefs["as.factor(Exams)70"],
               coefs["as.factor(Exams)75"], coefs["as.factor(Exams)80"], coefs["as.factor(Exams)85"], coefs["as.factor(Exams)90"])
coef_prob = c(coefs["as.factor(P_Collision)0.2"], coefs["as.factor(P_Collision)0.3"], coefs["as.factor(P_Collision)0.4"], coefs["as.factor(P_Collision)0.5"], coefs["as.factor(P_Collision)0.6"],
              coefs["as.factor(P_Collision)0.7"], coefs["as.factor(P_Collision)0.8"], coefs["as.factor(P_Collision)0.9"], coefs["as.factor(P_Collision)1"])

coef_test = t.test(coef_exams, coef_prob)
print(coef_test)

comparisons = TukeyHSD(two_way_anova)

two_way_interaction_stats = comparisons[["as.factor(Exams):as.factor(P_Collision)"]]
two_way_interaction_stats = data.frame(row.names(two_way_interaction_stats), two_way_interaction_stats[ ,4])
colnames(two_way_interaction_stats) = c("interactions", "p_value")
two_way_sig_stats = two_way_interaction_stats[two_way_interaction_stats$p_value <= 0.05, ]
two_way_non_sig_stats = two_way_interaction_stats[two_way_interaction_stats$p_value > 0.05, ]
#capture.output(comparisons[["as.factor(Exams):as.factor(P_Collision)"]],file="2way_interaction_posthocs.doc")
x11()
par(mfrow=c(2,2))
plot(three_way_anova)

x11()
par(mfrow=c(2,2))
plot(two_way_anova)

x11()
interaction.plot(outputdata$Exams, outputdata$P_Collision, outputdata$CPU_Time, xlab = "Number of Exams", ylab = "CPU Time", col = c(brewer.pal(n = 10, name = 'Paired')), trace.label = "Probability of Collision", lty = 1, lwd = 2)


x11()
interaction.plot(outputdata$P_Collision, outputdata$Exams, outputdata$CPU_Time, xlab = "Probability of Collision", ylab = "CPU Time", col = c(brewer.pal(n = 17, name = 'Paired')), trace.label = "Number of Exams", lty = 1, lwd = 2)
