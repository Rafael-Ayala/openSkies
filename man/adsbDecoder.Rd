\name{adsbDecoder}
\alias{adsbDecoder}
\title{An \code{\link{R6Class}} object representing a decoder used to decode ADS-B v2
messages}
\description{
\code{\link{R6Class}} object representing a decoder used to decode ADS-B v2 messages. Provides 
methods for decoding a single message or a batch. Additionally, it includes methods for transforming
hex strings into bits vectors and decoding some individual fields. Decoded messages are returned as
lists with each decoded field.
For ease of use, an already created instance, \code{adsbDec}, can be used out of the box.
}
\usage{
adsbDecoder
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

adsbDec <- adsbDecoderClass$new()

msg0 <- adsbDecoder$hexToBits("8D40621D58C386435CC412692AD6")
msg1 <- adsbDecoder$hexToBits("8D40621D58C382D690C8AC2863A7")
msg2 <- adsbDecoder$hexToBits("8D4840D6202CC371C32CE0576098")

decoded0 <- adsbDecoder$decodeMessage(msg0)
decoded1 <- adsbDecoder$decodeMessage(msg1)
decoded2 <- adsbDecoder$decodeMessage(msg2)

msgs <- sapply(list(msg0, msg1, msg2), hexToBits, simplify=FALSE)
decodedAll <- adsbDec$decodeMessages(msgs)
}
