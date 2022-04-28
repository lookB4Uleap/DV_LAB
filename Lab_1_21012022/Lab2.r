library(lattice)
attach(mtcars)
mtcars
# create factors with value labels
gear.f<-factor(gear,levels=c(3,4,5),
               labels=c("3gears","4gears","5gears"))
cyl.f <-factor(cyl,levels=c(4,6,8),
               labels=c("4cyl","6cyl","8cyl"))
#xy Scatter plot

xyplot(mpg~wt)
# scatterplots for each combination of two factors
xyplot(mpg~wt|cyl.f*gear.f,
       main="Scatterplots by Cylinders and Gears",
       ylab="Miles per Gallon", xlab="Car Weight")
# XYplot

xyplot(mpg~wt,
       data = mtcars,
       type = c("p", "r"),
       main = "Relation between wt and mpg",
       xlab = "Weight in lbs",
       ylab = "Miles/Gallon (US)")

xyplot(mpg~wt | cyl.f,
       type = c("p", "r"),
       groups = cyl.f,
       main = "Relation between wt and mpg over cylinders",
       xlab = "Weight in lbs",
       ylab = "Miles/Gallon (US)")
#Multivariate xy scatter plot with customizations

xyplot(mpg~wt | cyl.f,
       type = c("p", "r"),
       groups = cyl.f,
       main = "Relation between wt and mpg over cylinders",
       xlab = "Weight in lbs",
       ylab = "Miles/Gallon (US)")


# kernel density plot
densityplot(~mpg,
            main="Density Plot",
            xlab="Miles per Gallon")
# kernel density plots by factor level
densityplot(~mpg|cyl.f,
            main="Density Plot by Number of Cylinders",
            xlab="Miles per Gallon")
# kernel density plots by factor level (alternate layout)
densityplot(~mpg|cyl.f,
            main="Density Plot by Numer of Cylinders",
            xlab="Miles per Gallon",
            layout=c(1,3))
#Kernel Density Plot together for all factors without points

densityplot(~mpg,
            groups = gear.f,
            plot.points = FALSE,
            main = "Kernel density plot over number of gears",
            xlab = "Miles Per Gallon (US)")
# Boxplot
bwplot(gear.f ~ mpg | cyl.f,
       xlab = "Miles per Gallon (US)",
       ylab = "No of Gears",
       Main = "Mileage by no. of gears and cylinders")
# boxplots for each combination of two factors
bwplot(cyl.f~mpg|gear.f,
       ylab="Cylinders", xlab="Miles per Gallon",
       main ="Mileage by Cylinders and Gears")
#Boxplot associated with multiple variables and alternate layout

bwplot(gear.f ~ mpg |cyl.f,
       xlab = "Miles per Gallon (US)",
       ylab = "No of Gears",
       Main = "Mileage by no. of gears and cylinders",
       layout = c(1, 3))


# 3 d plot
cloud(mpg~wt*qsec,main = "3D scatterplot")
# 3d scatterplot by factor level
cloud(mpg~wt*qsec|cyl.f,
      main="3D Scatterplot by Cylinders")
# dotplot for each combination of two factors
dotplot(cyl.f~mpg|gear.f,
        main="Dotplot Plot by Number of Gears and Cylinders",
        xlab="Miles Per Gallon")
mtcars
# scatterplot matrix
splom(mtcars[c(1,3,4,5,6)],
      main="MTCARS Data")
bwplot(gear.f ~ mpg | cyl.f,
       data = mtcars,
       xlab = "Miles per Gallon (US)",
       ylab = "No of Gears",
       Main = "Mileage by no. of gears and cylinders",
       panel = panel.violin)
# contour plot
data <- dimnames(volcano)
contour(x=volcano, xlab = "Row", ylab = "Column") # Contour plot for volcano dataset
filled.contour(volcano)

filled.contour(volcano,color.palette = terrain.colors)