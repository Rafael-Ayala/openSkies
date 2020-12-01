\name{ADSBDecoder}
\alias{ADSBDecoder}
\title{An object of class \code{adsbDecoder} object representing a decoder used 
to decode ADS-B v2 messages}
\description{
\code{\link{R6Class}} object of class \code{adsbDecoder} representing a decoder 
used to decode ADS-B v2 messages. Provides methods for decoding a single message 
or a batch. Additionally, it includes methods for transforming hex strings into 
bits vectors and decoding some individual fields. Decoded messages are returned 
as lists with each decoded field.
}
\usage{
ADSBDecoder
}
\section{Fields}{

\describe{
  \item{\code{lastOddPosition}}{Last ground or airborne position message decoded with 
  \code{decodeGroundPositionMessage} or \code{decodeAirbornePositionMessage}
  }
  \item{\code{lastEvenPosition}}{Last ground or airborne position message decoded with 
  \code{decodeGroundPositionMessage} or \code{decodeAirbornePositionMessage}
  }
}}

\section{Methods}{

\describe{
  \item{
    \code{hexToBits(hex)}}{
     Transform a hexadecimal string into its corresponding bits representation, with higher 
bits in the first positions.
   },
   \item{
    \code{decodeCPR(cprLatEven, cprLonEven, cprLatOdd, cprLonOdd, isAirborne=TRUE)}}{
     Decodes a pair of CPR-encoded positions given as longitudes and latitudes,
corresponding to a pair of even and odd messages, obtaining the actual positions
for both. The isAirborne argument indicates whether or not the CPR-encoded positions correspond to 
isAirborne position messages or not (ground position messages). The result is given as a vector with
the decoded positions for both the even and odd messages, in the following order: even latitude, 
even longitude, odd latitude, odd longitude.
   },
   \item{
    \code{decodeMessage(message)}}{
     Decodes a single ADS-B v2 message in the form of a bits vector (higher bits 
in the first positions). The last even and odd positional messages are cached to
decode following positional messages. The following message types are supported: 
aircraft identification, airborne position, ground position, airborne velocity,
and operation status.
   },
   \item{
    \code{decodeMessages(messages)}}{
     Decodes several ADS-B v2 messages in the form of a list of bits vector (higher bits 
in the first positions). The following message types are supported: aircraft 
identification, airborne position, ground position, airborne velocity, and operation
status.
   }
}
}


\examples{

# Decode three messages, using both individual decoding and batch decoding. 
# The two first messages contain the airborne position.
# The third one, the aircraft identification

msg0 <- ADSBDecoder$hexToBits("8D40621D58C386435CC412692AD6")
msg1 <- ADSBDecoder$hexToBits("8D40621D58C382D690C8AC2863A7")
msg2 <- ADSBDecoder$hexToBits("8D4840D6202CC371C32CE0576098")

decoded0 <- ADSBDecoder$decodeMessage(msg0)
decoded1 <- ADSBDecoder$decodeMessage(msg1)
decoded2 <- ADSBDecoder$decodeMessage(msg2)

decodedAll <- ADSBDecoder$decodeMessages(list(msg0, msg1, msg2))
}
