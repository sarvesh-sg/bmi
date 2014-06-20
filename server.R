getRule <- function(country) {
  msg <- data.frame()
  switch(country,
         "1"={
           msg <- rbind(c("#Hong Kong","Status","Criteria"))
           msg <- rbind(msg,c("","Underweight","< 18.5"))
           msg <- rbind(msg,c("","#Normal Range",  "18.5 - 22.9")) 
           msg <- rbind(msg,c("","#Overweight - At Risk",  "23.0 - 24.9"))
           msg <- rbind(msg,c("","#Overweight - Moderately Obese",  "25.0 - 29.9"))
           msg <- rbind(msg,c("","#Overweight - Severely Obese"," ≥ 30.0"))
         },
         "2" = {
           msg <- rbind(c("#Japan","Status","Criteria"))
           msg <- rbind(msg,c("","#Low", " 18.5 and below"))
           msg <- rbind(msg,c("","#Normal"," from 18.5 to 25.0 (Standard weight is 22)"))
           msg <- rbind(msg,c("","#Obese (Level 1)"," from 25.0 to 30.0"))
           msg <- rbind(msg,c("","#Obese (Level 2)"," from 30.0 to 35.0"))
           msg <- rbind(msg,c("","#Obese (Level 3)"," from 35.0 to 40.0"))
           msg <- rbind(msg,c("","#Obese (Level 4)"," 40.0 and above"))
         },
         "3"={
           msg <- rbind(c("#Singapore","Status","Criteria"))
           msg <- rbind(msg,c("","High risk of developing heart disease, high blood pressure, stroke, diabetes", "#27.5 and above "))
           msg <- rbind(msg,c("","Moderate risk of developing heart disease, high blood pressure, stroke, diabetes", "#23.0 to 27.4 "))
           msg <- rbind(msg,c("","Low Risk (healthy range)", "#18.5 to 22.9 "))
           msg <- rbind(msg,c("","Risk of developing problems such as nutritional deficiency and osteoporosis", "#18.4 and below "))
              },
         "4"={
           msg <- rbind(c("#Others","Status","Criteria"))
           msg <- rbind(msg,c("","#Very severely underweight"," less than 15"))
           msg <- rbind(msg,c("","#Severely underweight"," from 15.0 to 16.0"))
           msg <- rbind(msg,c("","#Underweight"," from 16.0 to 18.5"))
           msg <- rbind(msg,c("","#Normal (healthy weight)"," from 18.5 to 25"))
           msg <- rbind(msg,c("","#Overweight"," from 25 to 30"))
           msg <- rbind(msg,c("","#Obese Class I (Moderately obese)","from 30 to 35"))
           msg <- rbind(msg,c("","#Obese Class II (Severely obese)"," from 35 to 40"))
           msg <- rbind(msg,c("","#Obese Class III (Very severely obese)"," over 40"))
         }
  )
  msg
  
}
getBmi <- function(height,weight) {
  heightInMeter <- height*.01
  bmi <- weight/heightInMeter^2  
  bmi
}
diagnosis <- function(bmi) {
  #BMI for Adults(Above 20 years):
  if (bmi < 15) status <- "Very severely underweight"
  else if (bmi >= 15 & bmi <= 16) status <- "Severely underweight"
  else if (bmi >= 16 & bmi <= 18.5) status <- "Underweight"
  else if (bmi >= 18.5 & bmi <= 25) status <- "Normal (healthy weight)"
  else if (bmi >= 25 & bmi <= 30) status <- "Overweight"
  else if (bmi >= 30 & bmi <= 35) status <- "Obese Class I (Moderately obese)"
  else if (bmi >= 35 & bmi <= 40) status <- "Obese Class II (Severely obese)"
  else if (bmi > 40) status <- "Obese Class III (Very severely obese)"
  status
}

getAgeMsg <- function(age){
  if (age<20){
    msg <- "#BMI for age below 20 years
      #Overweight: >+1SD
      #Obesity: >+2SD 
      #Thinness: <-2SD
      #Severe thinness: <-3SD"
  } else {
    msg <- "BMI for Adults"
  }
  msg
}
getStatus <- function(age,gender,country,height,weight) {
    bmi <- getBmi(height,weight)
    refBMI <- read.csv("below20RefbmI.csv",header=TRUE,sep=";")
    names(refBMI) <- c("gender","age","m2sd","m1.5sd","m1sd","m.5sd","mid","p.5sd","p1sd","p1.5sd","p2sd")
    status <- "Not Defined"
    healthRisk <- "No recommendations"
    if(age<20) {
      months <- age*12;
      if (gender == "Male")
        bmiRange <- refBMI[refBMI$age==months & refBMI$gender==1,]
      else 
        bmiRange <- refBMI[refBMI$age==months & refBMI$gender==2,]
      if (bmi > bmiRange[1,"p2sd"]) status <- "Obesity"
      else if (bmi > bmiRange[1,"p1sd"] ) status <- "Overweight"
      else if (bmi < bmiRange[1,"m2sd"]) status <- "Thinness"
      else if (bmi < bmiRange[1,"m2sd"]) status <- "Severe Thinness"
    } else {
      switch(country,
             "1"={
                  if (bmi < 18.5) status <- "Underweight"
                  else if (bmi >= 18.5 & bmi <= 22.9) status <- "Normal Range"
                  else if (bmi >= 23 & bmi <= 24.9) status <- "Overweight - At Risk"
                  else if (bmi >= 25 & bmi <= 29.9) status <- "Overweight - Moderately Obese"
                  else if (bmi >= 30) status <- "Overweight - Servely Obese"
             },
             "2" = {
               if (bmi < 18.5) status <- "Low Weight"
               else if (bmi >= 18.5 & bmi <= 25) status <- "Normal Range"
               else if (bmi >= 25 & bmi <= 30) status <- "Obese (Level 1)"
               else if (bmi >= 30 & bmi <= 35) status <- "Obese (Level 2)"
               else if (bmi >= 35 & bmi <= 40) status <- "Obese (Level 3)"
               else if (bmi > 40) status <- "Obese (Level 4)"
             },
             "3" = {
               status <- diagnosis(bmi)
               if (bmi < 18.4) healthRisk <- "Risk of developing problems such as nutritional deficiency and osteoporosis"
               else if (bmi >= 18.5 & bmi <= 22.9) healthRisk <- "Low Risk (healthy range)"
               else if (bmi >= 23 & bmi <= 27.4) healthRisk <- "Moderate risk of developing heart disease, high blood pressure, stroke, diabetes"
               else if (bmi >= 27.5) healthRisk <- "High risk of developing heart disease, high blood pressure, stroke, diabetes"
             },
             "4" = { 
               status <- diagnosis(bmi)
             }
        )
    }
    status
}    
shinyServer(
  function(input,output){
    output$bmi <- renderPrint({getBmi(input$height,input$weight)})
    output$age <- renderPrint({input$age})
    output$status <- renderPrint({getStatus(input$age,input$gender,input$country,input$height,input$weight)})
    output$msg <- renderTable({getRule(input$country)})
  }
  )