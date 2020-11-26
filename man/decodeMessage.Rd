\name{decodeMessage}
\alias{decodeMessage}
\title{Decode an ADS-B v2 message
}
\description{
Decodes a single ADS-B v2 message in the form of a bits vector (higher bits 
in the first positions). The last even and odd positional messages are cached to
decode following positional messages. The following message types are supported: 
aircraft identification, airborne position, ground position, airborne velocity,
and operation status.
}
\usage{
decodeMessage(message)
}
\arguments{
  \item{message}{Bits vector corresponding to an ADS-B v2 message.
  }
}
\value{
A list with the fields of the decoded message. Data fields are contained in the "data"
list field.
}

\examples{
# Decode three messages. The two first messages contain the airborne position. Only the
# result for the second one will include the decoded latitude and longitude. The third
# one contains aircraft identification

msg0 <- hexToBits("8D40621D58C386435CC412692AD6")
msg1 <- hexToBits("8D40621D58C382D690C8AC2863A7")
msg2 <- hexToBits("8D4840D6202CC371C32CE0576098")

decoded0 <- decodeMessage(msg0)
decoded1 <- decodeMessage(msg1)
decoded2 <- decodeMessage(msg2)

}
