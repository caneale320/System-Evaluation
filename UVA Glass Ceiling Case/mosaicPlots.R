library(vcd)
data = read.csv("pub_recoded_school.csv")

gender_title = table(data$gender, data$title)
mosaicplot(gender_title, shade= TRUE, las=2,
           main = "Gender and Title at UVA", legend= TRUE)

gender_school = table(data$gender, data$Recoded.school)
mosaicplot(gender_school, shade= TRUE, las=2,
           main = "Gender and School of Appointment at UVA", legend= TRUE)
