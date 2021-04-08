\name{dml_bagging}
\alias{dml_bagging}

\title{
Double Machine Learning based on bagging
}
\description{
The most famous representative of parallel ensemble learning. This method uses the self-help method to repeatedly sample from a single training set and generate several different self-help sampling training sets. Then, the self-help sampling training sets are used to fit the model and then the predicted values are obtained.
}
\usage{
dml_bagging(y,x,d,data,sed)
}

\arguments{
  \item{y,x,d,data,sed}{
}
}

\value{
y   Dependent variable;

d  Independent variable;

x  Control variables;

sed    A random seed;

data    Data
}
\references{
Leo Breiman. (1996). Bagging Predictors. Machine Learning, 24(2), pp. 123-140. doi: 10.1023/A:1018054314350

Jui-Chung Yang,,Hui-Ching Chuang & Chung-Ming Kuan.(2020).Double machine learning with gradient boosting and its application to the Big N audit quality effect. Journal of Econometrics(1),.doi:10.1016/j.jeconom.2020.01.018

Victor Chernozhukov,,Denis Chetverikov,,Mert Demirer,... & James Robins.(2018).Double/debiased machine learning for treatment and structural parameters. The Econometrics Journal(1),. doi:10.3386/w23564.

}
\author{
 Lixiong Yang<ylx@lzu.edu.cn>
}




\seealso{
 https://github.com/lixiongyang
}

\examples{

library(ISLR)
attach(Auto)
data<- Auto
y <- data$mpg    #Dependent variable
d <- data$origin   #Independent variable
x="weight+year +horsepower"      #Control variables;

dml_bagging(y,x,d,data,sed=123)

}
