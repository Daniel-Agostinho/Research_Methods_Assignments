# clean console, remove variables and close plots =============================

cat("\014")
rm(list=ls())
while (!is.null(dev.list())){
  dev.off()
}

options(device = "windows") # plots in pop up windows

# set directory
setwd("C:\\Users\\danie\\Desktop\\PhD\\1ºSemestre\\Research_Methods")
#setwd('C:\\Users\\Julia\\Documents\\PhD - EB\\RM\\Assignment_DA_JS')

# libraries

library(ggplot2)
library(rstatix)
library(RColorBrewer)
library(dplyr)

# reading data
outputdata = read.table("dataset.txt", header = TRUE, dec = ".")

# functions
get_data_by_code_type = function(data_set, code_type){
  new_data_set = data_set[data_set$Code_Type == code_type, ]
  var_names = c('Exams', 'CPU_Time', 'P_Collision', 'Collisions', "Slots")
  cpu_time = new_data_set$CPU_Time
  cpu_time[cpu_time > 300] = 300.0
  output = data.frame(new_data_set$Number_of_Exams, cpu_time, new_data_set$Collision_Prob, new_data_set$Collisions, new_data_set$Number_Slots)
  names(output) = var_names
  return(output)
}

select_data_by_exam = function(data_set, exam_number){
  return(data_set[data_set$Exams == exam_number, ])
}

select_data_by_prob = function(data_set, prob_number){
  return(data_set[data_set$P_Collision == prob_number, ])
}

compare_code_type = function(data_set){
  
  code1 = get_data_by_code_type(data_set, 'code1')
  code2 = get_data_by_code_type(data_set, 'code2')
  
  x11()
  plot(code1$CPU_Time, code2$CPU_Time, main = "Comparison between the two codes", 
       xlab = "Code1 CPU Times", ylab = "Code2 CPU Times", pch = 19, xlim = c(0, 301), ylim = c(0, 301))
  
  
  
  regress = lm(code2$CPU_Time ~ code1$CPU_Time)
  abline(regress, col="red", lwd = 2)
  r_value = sqrt(summary(regress)$r.squared)
  text(100, 30,  paste(sprintf("r = %.4f", r_value)))
  
  return(regress)
  
}


plot_data_hist = function(data_set, code_type){
  
  new_data_set = get_data_by_code_type(data_set, code_type)
  exams_set = unique(new_data_set$Exams)
  
  for (exam in exams_set) {
    x11()
    par(mfrow=c(2,5))
    data_hist(new_data_set, exam)
    
  }
  
  
}


data_hist = function(data_set, number_of_exams){
  prob_set = unique(data_set$P_Collision)
  for (prob in prob_set) {
    plot_data = select_data_by_prob(select_data_by_exam(data_set, number_of_exams), prob)
    histogram = hist(plot_data$CPU_Time,   plot = FALSE)
    histogram$counts=histogram$counts/sum(histogram$counts)
    plot(histogram, xlab = "CPU Time", main = sprintf("Exam = %i Probability = %.2f", number_of_exams, prob), col = c(brewer.pal(n = 8, name = 'Pastel1')))
    
  }
  
  }


data_description = function(data_set, code_type){
  
  data_code = get_data_by_code_type(data_set, code_type)
  table1 = summary(data_code)
  print(table1)
  
  
  p_collisions_set = unique(data_code$P_Collision)
  
  x11()
  par(mfrow=c(2,5))
  for (p_collision in p_collisions_set) {
    
    new_data_code = select_data_by_prob(data_code, p_collision)
    boxplot(CPU_Time ~ Exams, data = new_data_code, main = sprintf("Probability of colision %.2f", p_collision), 
         xlab = "Number of Exams", ylab = "CPU Time", col = c(brewer.pal(n = 9, name = 'Pastel1')))
  }
  
  exams_set = unique(data_code$Exams)
  
  x11()
  par(mfrow = c(4,5)) # mudar para o set final
  for (exam in exams_set) {
    new_data_code = select_data_by_exam(data_code, exam)
    boxplot(CPU_Time ~ P_Collision, data = new_data_code, main = sprintf("Number of exams %i", exam), 
            xlab = "Probability of collision", ylab = "CPU Time", col = c(brewer.pal(n = 9, name = 'Pastel1')))
  }
  
}

concluded_cases = function(data_set, code_type){
  
  data_code = get_data_by_code_type(data_set, code_type)
  exams_set = unique(data_code$Exams)
  prob_set = unique(data_code$P_Collision)
  
  x11()
  par(mfrow=c(5,4))
  for (exam in exams_set) {
    
    conclusion_rates = c()
    for (prob in prob_set) {
      
      conclusion_rate = get_conclusion_rate(data_code, exam, prob)
      conclusion_rates = c(conclusion_rates, conclusion_rate)
      
    }
    bp = barplot(conclusion_rates, names = prob_set, main = sprintf("Number of exams %i", exam), col = c(brewer.pal(n = 10, name = 'Pastel1')), xlab = "Probability of collision", ylab = "Conclusion rate (%)")
    text(bp, 0, round(conclusion_rates, 1),cex=1,pos=3) 
  }
  
}



get_conclusion_rate = function(data_set, number_of_exams, probability_number){

  
  family_dataset = select_data_by_prob(select_data_by_exam(data_set, number_of_exams), probability_number)
  total_cases = length(family_dataset$Slots)
  not_concluded = sum(family_dataset$Slots == -1)
  concluded = ((total_cases-not_concluded)/total_cases)*100
  return(concluded)
}

mean_cpu_time_plot = function(data_set, code_type, indvar = "Exams", transform_vars=FALSE){
  
  
  if (indvar == "Exams") {
    return(mean_cpu_time_exams(data_set, code_type, transform_vars))
  }
  else if (indvar == "Probs"){
    return(mean_cpu_time_probs(data_set, code_type, transform_vars))
  }
  else{
    print("Specify the indvar as Exams or Probs")
  }
  
}

mean_cpu_time_exams = function(data_set, code_type, transform_var){
  data_code = get_data_by_code_type(data_set, code_type)
  exams_set = unique(data_code$Exams)
  prob_set = unique(data_code$P_Collision)
  places = list(c(70, 150),c(60, 150),c(55, 150),c(55,150),c(50,150), 
                c(45, 150),c(45,150),c(40,155),c(40, 200),c(40, 240))
  
  if (transform_var) {
    places = list(c(70, 10),c(60, 10),c(55, 10),c(55,10),c(50,10), 
                  c(45, 10),c(45,10),c(40,10),c(40, 12),c(40, 12))
  }
  indx = 1
  
  regressions = c()
  
  x11()
  par(mfrow=c(2,5))
  for (prob in prob_set) {
    mean_cpu_time = get_mean_cpu_time(data_code, prob, exams_set, "Exams")
    
    if (transform_var) {
      mean_cpu_time = sqrt(mean_cpu_time)
    }
    
    plot(exams_set, mean_cpu_time, xlab = "Number of Exams", ylab = "Mean CPU Time",
         main = sprintf("Probability of colision %.2f", prob), pch = 19)
    regress = lm(mean_cpu_time ~ exams_set)
    regressions = c(regressions, list(regress))
    abline(regress)
    r_value = sqrt(summary(regress)$r.squared)
    x_pos = places[[indx]][1]
    print(x_pos)
    y_pos = places[[indx]][2]
    text(x_pos, y_pos,  paste(paste(sprintf("r = %.4f", r_value))))
    indx = indx + 1
  }
  
  return(regressions)
  
}

mean_cpu_time_probs = function(data_set, code_type, transform_var){
  data_code = get_data_by_code_type(data_set, code_type)
  exams_set = unique(data_code$Exams)
  prob_set = unique(data_code$P_Collision)
  places = list(c(0.5, 0.01), c(0.5, 65), c(0.5, 100), c(0.5, 150), c(0.4, 150), c(0.6, 150), c(0.5, 150), c(0.5, 150), 
             c(0.5, 190), c(0.5, 210), c(0.5, 230), c(0.5, 260), c(0.5, 285), c(0.3, 296), c(0.5, 250), c(0.5, 250), c(0.5, 250))
  if (transform_var) {
    places = list(c(0.5, 0.10), c(0.5, 5), c(0.4, 5), c(0.5, 8), c(0.4, 10), c(0.6, 10), c(0.5, 10), c(0.5, 11),
                  c(0.5, 12), c(0.5, 12), c(0.5, 14), c(0.5, 16), c(0.5, 17), c(0.3, 17.20), c(0.5, 14), c(0.5, 14), c(0.5, 14))
  }
  indx = 1
  regressions = c()
  
  x11()
  par(mfrow=c(4,5))
  for (exam in exams_set) {
    mean_cpu_time = get_mean_cpu_time(data_code, exam, prob_set, "Probs")
    if (transform_var) {
      mean_cpu_time = sqrt(mean_cpu_time)
    }
    plot(prob_set, mean_cpu_time, xlab = "Probability of Collision", ylab = "Mean CPU Time",
         main = sprintf("Number of Exams %i", exam), pch = 19)
    regress = lm(mean_cpu_time ~ prob_set)
    regressions = c(regressions, list(regress))
    abline(regress)
    r_value = sqrt(summary(regress)$r.squared)
    x_pos = places[[indx]][1]
    y_pos = places[[indx]][2]
    text(x_pos, y_pos,  paste(paste(sprintf("r = %.4f", r_value))))
    indx = indx + 1
  }
  
  return(regressions)
  
  
}


get_mean_cpu_time = function(data_set, locvar, set, indvar){
  

  mean_cpu_time = c()
  for (ivar in set) {
    
    if (indvar == "Exams") {
      cpu_times = select_data_by_exam(select_data_by_prob(data_set, locvar), ivar)
    }
    else if (indvar == "Probs"){
      cpu_times = select_data_by_prob(select_data_by_exam(data_set, locvar), ivar)
    }
    mean_cpu_time = c(mean_cpu_time, mean(cpu_times$CPU_Time))
  }
  return(mean_cpu_time)
}

better_performamce = function(dataset, code1, code2){
  data1 = get_data_by_code_type(dataset, code1)
  data2 = get_data_by_code_type(dataset, code2)
  size = length(data1$CPU_Time)
  code1_code2 = c()
  
  for (i in c(1:size)) {
    if (data1$CPU_Time[i] > data2$CPU_Time[i]) {
      value_teste = data1$CPU_Time[i] - data2$CPU_Time[i]
      if(between(value_teste, 0, 20)){
        code1_code2 = c(code1_code2, 0)
      }
      else{
        code1_code2 = c(code1_code2, 1)
      }
      
    }
    
    else if (data1$CPU_Time[i] < data2$CPU_Time[i]) {
      value_teste = data1$CPU_Time[i] - data2$CPU_Time[i]
      if(between(value_teste, -20, 0)){
        code1_code2 = c(code1_code2, 0)
      }
      else{
        code1_code2 = c(code1_code2, -1)
      }
    }
    
    else{
      code1_code2 = c(code1_code2, 0)
    }
    
  }
  
  print(sprintf("Code2 faster than Code1 = %i",sum(code1_code2 == 1)))
  print(sprintf("Code1 faster than Code2 = %i",sum(code1_code2 == -1)))
  print(sprintf("They are equal = %i",sum(code1_code2 == 0)))
  
}


assumpion_plot = function(regressions){
  
  for (regress in regressions) {
    x11()
    par(mfrow=c(2,2))
    plot(regress)
  }
  
}

code_type = "code2"

#compare_code_type(outputdata)
#plot_data_hist(outputdata, code_type)

#data_description(outputdata, code_type)

#concluded_cases(outputdata, code_type)
regressions = mean_cpu_time_plot(outputdata, code_type,"Probs", transform_vars = TRUE)

#assumpion_plot(regressions)
#code_data = get_data_by_code_type(outputdata, code_type)

#better_performamce(outputdata, "code1", "code2")
#hist(code_data$CPU_Time)
