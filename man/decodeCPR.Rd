\name{decodeCPR}
\alias{decodeCPR}
\title{Decode a pair of CPR-encoded positions (even and odd)
}
\description{
Decodes a pair of CPR-encoded positions given as longitudes and latitudes,
corresponding to an even and an odd message, obtaining the actual positions
for both.
}
\usage{
decodeCPR(cprLatEven, cprLonEven, cprLatOdd, cprLonOdd, isAirborne=TRUE)
}
\arguments{
  \item{cprLatEven}{CPR-encoded latitude of the even position message,
   obtained directly from the message bits.
  }
  \item{cprLonEven}{CPR-encoded longitude the an even position message.
   obtained directly from the message bits.
  }
  \item{cprLatOdd}{CPR-encoded latitude of the odd position message.
   obtained directly from the message bits.
  }
  \item{cprLonOdd}{CPR-encoded longitude of the odd position message.
   obtained directly from the message bits.
  }
  \item{isAirborne}{Whether or not the CPR-encoded positions correspond to isAirborne
  position messages or not (ground position messages).
  }
}
\value{
A vector with the decoded positions for both the even and odd messages, in the following
order: even latitude, even longitude, odd latitude, odd longitude.
}

\examples{
# Obtain the decoded latitudes for a couple 
# of airborne position messages.

decodeCPR(93000, 51382, 74158, 50194)
}
