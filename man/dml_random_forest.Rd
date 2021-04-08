\name{dml_random_forest}
\alias{dml_random_forest}

\title{
Double Machine Learning based on random forest
}
\description{
To establish a series of decision tree, the difference is the this method for each division point considering the decision tree, should be chosen from among all variables contain random sample with some of the variables as candidate variables, the explanatory variables can only be used in the split point from the selected part of the explanation variable selection, and then to make predictions.
}
\usage{
dml_random_forest(y,x,d,data,sed)
}

\arguments{
  \item{y,x,d,data,sed}{

}
}

\value{
y   Dependent variable;

d   Independent variable;

x   Control variable;

sed   A random seed;

data  Data
}
\references{
Leo Breiman. (2001). Random Forests. Machine Learning, 45(1), pp. 5-32. doi:10.1023/A:1010933404324

Jui-Chung Yang,,Hui-Ching Chuang & Chung-Ming Kuan.(2020).Double machine learning with gradient boosting and its application to the Big N audit quality effect. Journal of Econometrics(1).doi:10.1016/j.jeconom.2020.01.018
Victor Chernozhukov,,Denis Chetverikov,,Mert Demirer,... & James Robins.(2018).Double/debiased machine learning for treatment and structural parameters. The Econometrics Journal(1). doi:10.3386/w23564.
}
\author{
 Lixiong Yang<ylx@lzu.edu.cn>
}


\examples{
library(ISLR)
attach(Auto)
data<- Auto
y <- data$mpg    #Dependent variable
d <- data$origin   #Independent variable
x="weight+year +horsepower"      #Control variables;

dml_random_forest(y,x,d,data,sed=123)
}

