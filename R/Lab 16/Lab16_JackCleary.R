X <- read.table( "/Users/jackcleary/Desktop/Stats 3rd Year/Lab 12/diabetes.csv", sep=",", header=T, na.strings="?" )
cor(X)

ggplot(X, aes(x = BMI, y = DiabetesPedigreeFunction)) +
  geom_point(aes(col = Age), size = 4, shape = paste("circle","cross"), stroke = 1.2) +
  geom_smooth(method= "lm", linetype = "twodash", color = "red4", fill = "red") +
  scale_color_gradient(low = "skyblue", high = "blue3", limits = c(20, 80))+
  xlim(15, 60) +
  ylim(0, 1.8) +
  labs(title = "Relationship between BMI & Diabetes Pedigree Function", 
       subtitle = "from Pima Indians Diabetes Dataset", 
       caption = "Red line represents Linear model and shaded area shows Confidence bands") +
  xlab("BMI") +
  ylab("Diabetes Pedigree Function")
  
