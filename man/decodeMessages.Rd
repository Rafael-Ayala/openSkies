\name{decodeMessages}
\alias{decodeMessages}
\title{Decode a list of ADS-B v2 messages
}
\description{
Decodes several ADS-B v2 messages in the form of a list of bits vector (higher bits 
in the first positions). The following message types are supported: aircraft 
identification, airborne position, ground position, airborne velocity, and operation
status.
}
\usage{
decodeMessages(messages)
}
\arguments{
  \item{messages}{List of vectors of bits corresponding to ADS-B v2 messages.
  }
}
\value{
A list with each decoded message. Each element is a list with the fields of the
decoded message. Data fields are contained in the "data" list field.
}

\examples{
# Decode three messages. The two first messages contain the airborne position.
# The third one, the aircraft identification

msgs <- sapply(list("8D40621D58C386435CC412692AD6",
                    "8D40621D58C382D690C8AC2863A7",
                    "8D4840D6202CC371C32CE0576098"),
              hexToBits,
              simplify=FALSE)

decoded <- decodeMessages(msgs)

}
