\name{predictIsing}
\alias{predictIsing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predicted values given an Ising model
}
\description{
For every node and every case, the expected value given the raw data of all other nodes in that case is returned. These predicted values can be used to compute cross-validation prediction error.
}
\usage{
predictIsing(net, thresh, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
A matrix encoding the Ising network
}
  \item{thresh}{
A vector of thresholds
}
  \item{data}{
Raw (binary) data
}
}
\author{
Sacha Epskamp <mail@sachaepskamp.com>
}

