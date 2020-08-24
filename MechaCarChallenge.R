mecha_data <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)                                                            

mecha_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data)

summary(mecha_lm)

#vehicle_length
model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data) #create linear model
yvals <- model$coefficients['vehicle_length']*mechacar_mpg$vehicle_length +
model$coefficients['(Intercept)'] #determine y-axis values from linear model

plt <- ggplot(mecha_data,aes(x=vehicle_length,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model


#vehicle_weight
model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data) #create linear model
yval <- model$coefficients['vehicle_weight']*mechacar_mpg$vehicle_weight +
model$coefficients['(Intercept)'] #determine y-axis values from linear model

plt <- ggplot(mecha_data,aes(x=vehicle_weight,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model


#spoiler_angle
model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data) #create linear model
yval1 <- model$coefficients['spoiler_angle']*mechacar_mpg$spoiler_angle +
model$coefficients['(Intercept)'] #determine y-axis values from linear model

plt <- ggplot(mecha_data,aes(x=spoiler_angle,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model


#ground_clearance
model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data) #create linear model
yval2 <- model$coefficients['ground_clearance']*mechacar_mpg$ground_clearance +
model$coefficients['(Intercept)'] #determine y-axis values from linear model

plt <- ggplot(mecha_data,aes(x=ground_clearance,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model


#AWD
model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data) #create linear model
yval3 <- model$coefficients['AWD']*mechacar_mpg$AWD +
model$coefficients['(Intercept)'] #determine y-axis values from linear model

plt <- ggplot(mecha_data,aes(x=AWD,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model





#Suspension Coil Summary 
suspension_coil <- read.csv(file="Suspension_Coil.csv", check.names=F, stringsAsFactors = F )

median(suspension_coil$PSI, na.rm=T)
mean(suspension_coil$PSI, na.rm=T)
sd(suspension_coil$PSI, na.rm=T)
var(suspension_coil$PSI, na.rm=T)





##SuspensionCoil T-Test
sample_table <- suspension_coil %>% sample_n(10) #generate 10 random sampled data points
sample_table2 <- suspension_coil %>% sample_n(10) # generate another 10 random sampled data points

t.test(log10(sample_table$PSI), log10 (sample_table2$PSI)) #compare means of two samples