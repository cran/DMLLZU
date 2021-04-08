\name{dml_boosting}
\alias{dml_boosting}

\title{
Double Machine Learning based on boosting
}
\description{
The biggest difference with other method, the trees of this  method are generated sequentially. Each tree is constructed using the information of the previous generated trees. Each tree is generated according to a modified version of the original data set, and finally these trees are combined to establish a prediction model
}
\usage{
dml_boosting(y,x,d,data,sed)
}

\arguments{
  \item{y,x,d,data,sed}{


}
}

\value{
y   Dependent variable;

d   Independent variable;

x   Control variable;

sed    A random seed;

data   Data
}
\references{
Jui-Chung Yang,,Hui-Ching Chuang & Chung-Ming Kuan.(2020).Double machine learning with gradient boosting and its application to the Big N audit quality effect. Journal of Econometrics(1),. doi:10.1016/j.jeconom.2020.01.018
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

dml_boosting(y,x,d,data,sed=123)
}
