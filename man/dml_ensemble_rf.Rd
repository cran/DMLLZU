\name{dml_ensemble_rf}
\alias{dml_ensemble_rf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dml_ensemble_rf
}
\description{
As an important integrated learning method, stacking  consists of at least two layers of structure, including a primary learner and a secondary learner or a meta-learner used to combine the learner. Stacking first trained the primary learner from the initial data set, and then generated a new data set used to train the secondary learner, in this data set, the output of the primary learner is taken as the sample input characteristics, and the initial sample mark is still taken as the sample mark.Integrate  the four  basic model through random forest
}
\usage{
dml_ensemble_rf(y,x,d,data,sed)
}

\arguments{
  \item{y,x,d,data,sed}{

}
}

\value{
y   Dependent variable;

d   Independent variable;

x  Control variable;

sed    A random seed;

data   Data
}
\references{
Jui-Chung Yang,,Hui-Ching Chuang & Chung-Ming Kuan.(2020).Double machine learning with gradient boosting and its application to the Big N audit quality effect. Journal of Econometrics(1),.doi:10.1016/j.jeconom.2020.01.018
Victor Chernozhukov,,Denis Chetverikov,,Mert Demirer,... & James Robins.(2018).Double/debiased machine learning for treatment and structural parameters. The Econometrics Journal(1),. doi:10.3386/w23564.
}
\author{
 Lixiong Yang<ylx@lzu.edu.cn>;
 Junchang Zhao <zhaojch19@lzu.edu.cn>
}



\seealso{
 \code{\link{help}}
}

\examples{
library(ISLR)
attach(Auto)
data<- Auto
y <- data$mpg    #Dependent variable
d <- data$origin   #Independent variable
x="weight+year +horsepower"      #Control variables;

dml_ensemble_rf(y,x,d,data,sed=123)
}
