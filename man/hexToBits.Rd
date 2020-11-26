\name{hexToBits}
\alias{hexToBits}
\title{Transforms a hexadecimal string into a raw bits vector
}
\description{
Transform a hexadecimal string into its corresponding bits representation, with higher 
bits in the first positions.
}
\usage{
hexToBits(hex)
}
\arguments{
  \item{hex}{Hexadecimal string.
  }
}
\value{
A raw bits vector with the bits representation of the input hexadecimal string.
}

\examples{
# Transform an hexadecimal string into bits

hexToBits("8D40621D58C386435CC412692AD6")

}
