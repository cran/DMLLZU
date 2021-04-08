\name{dml_neural_network}
\alias{dml_neural_network}

\title{
Double Machine Learning based on neural network
}
\description{

Each node represents a specific output function, known as the excitation function.It is a mathematical model or a computational model that imitates the structure and function of biological  net. It is calculated by the connection of a large number of artificial neurons, mainly composed of nodes and the mutual connections between nodes.
Each connection between two nodes represents a weighted value for the signal passing through the connection, known as the weight.
The output of the network is different according to the connection mode of the network, the weight value and the excitation function.
}
\usage{
dml_neural_network(y,x,d,data,sed)
}

\arguments{
  \item{y,x,d,data,sed}{


}
}

\value{
y    Dependent variable;

d    Independent variable;

x    Control variable;

sed   A random seed;

data    Data
}
\references{
Jui-Chung Yang,,Hui-Ching Chuang & Chung-Ming Kuan.(2020).Double machine learning with gradient boosting and its application to the Big N audit quality effect. Journal of Econometrics(1),. doi:10.1016/j.jeconom.2020.01.018

Victor Chernozhukov,,Denis Chetverikov,,Mert Demirer,... & James Robins.(2018).Double/debiased machine learning for treatment and structural parameters. The Econometrics Journal(1),. doi:10.3386/w23564.
}
\author{
 Yang Lixiong
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

dml_neural_network(y,x,d,data,sed=123)
}
