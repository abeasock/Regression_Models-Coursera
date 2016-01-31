### Exploratory Data Analysis
# Load the data set `mtcars`. Use `?mtcars` to view the help document on the datasset to find out more information on the `mtcars` dataset. From this we learn the mapping of the variable `am`: 0=automatic, 1=manual.
data(mtcars)
mtcars$am <- factor(mtcars$am, labels=c("automatic","manual"))


boxplot(mtcars$mpg~mtcars$am, xlab="Transmission", ylab="MPG", main="Boxplot of MPG vs Transmission")


