getTrinoToken <- function(username, password, client_id = NULL, client_secret = NULL) {
    if (!is.null(client_id) && !is.null(client_secret)){
        request <- POST(openskyAuthURL, encode="form",
                        body = list(grant_type = "client_credentials", client_id = client_id, client_secret = client_secret))
    }
    else {
        request <- POST(openskyAuthURL, encode="form",
                        body=list(grant_type="password", client_id="trino-client", username=username, password=password))
    }
  token <- content(request)$access_token
  return(token)
}

getTrinoConnection <- function(username, token) {
    set_config(
        add_headers(Authorization = paste("Bearer", token)),
        override = TRUE
        )
    con <- dbConnect(
        drv = RPresto::Presto(),
        host = "https://trino.opensky-network.org",
        port = 443,
        user = username,
        catalog = "minio",
        schema = "osky",
        use.trino.headers=TRUE)
    set_config(config())
    return(con)
}

getTrinoAuth <- function(username=NULL, password=NULL, client_id=NULL, client_secret=NULL){
  token <- getTrinoToken(username = username, password = password, client_id = client_id, client_secret = client_secret)
    set_config(
        add_headers(Authorization = paste("Bearer", token)),
        override = TRUE
        )
    con <- dbConnect(
        drv = RPresto::Presto(),
        host = "https://trino.opensky-network.org",
        port = 443,
        user = username,
        catalog = "minio",
        schema = "osky",
        use.trino.headers=TRUE)
    set_config(config())
  return(connection)
}

# Authentication abstraction layer

# Credentials object constructor
createCredentials <- function(method, ...) {
  args <- list(...)
  
  switch(method,
    basic = {
      list(
        method = "basic",
        username = args$username,
        password = args$password,
        token = NULL
      )
    },
    client_credentials = {
      list(
        method = "client_credentials",
        client_id = args$client_id,
        client_secret = args$client_secret,
        token = NULL
      )
    },
    stop("Unknown authentication method:  ", method)
  )
}

# Normalize different auth formats into a credentials object
getCredentials <- function(username = NULL, password = NULL,
                               client_id = NULL, client_secret = NULL) {
  # Check if basic auth provided
  if (!is.null(username) && !is.null(password)) {
    return(createCredentials("basic", username = username, password = password))
  }
  
  # Check if client credentials provided
  if (!is.null(client_id) && !is.null(client_secret)) {
    return(createCredentials("client_credentials", 
                           client_id = client_id, 
                           client_secret = client_secret))
  }
  
  # No auth provided
  return(NULL)
}

# Check if authentication is provided
hasAuth <- function(credentials) {
  ! is.null(credentials) && !is.null(credentials$method)
}

# Check if token exists and is valid
hasToken <- function(credentials) {
  !is.null(credentials) && !is.null(credentials$token)
}

# Abstracted authenticated HTTP request
makeAuthenticatedRequest <- function(request_url, query, credentials, timeOut = 60, maxQueryAttempts = 1) {
    # If url is just a path, prepend the opensky root URL
      if (! grepl("^https?://", request_url)) {
        request_url <- paste0(openskyApiRootURL, request_url)
      }

  jsonResponse <- FALSE
  attemptCount <- 0
  
  while (!jsonResponse) {
    attemptCount <- attemptCount + 1
    
    response <- tryCatch({
        if(hasAuth(credentials)){
            if (credentials$method == 'basic') {
                GET(request_url, query = query, timeout(timeOut),
                    authenticate(credentials$username, credentials$password))
            }else if(!hasToken(credentials)){
                token <- getTrinoToken(client_id = credentials$client_id,
                                       client_secret = credentials$client_secret)            
                credentials$token <- token
            }
            # credentials method must be client_credentials and we either had or obtained the token
            GET(request_url,query=query, timeout(timeOut),
                  add_headers(Authorization = paste("Bearer", credentials$token)))
        } else {
            GET(request_url, query = query, timeout(timeOut))
        }
    }, error = function(e) e)
    
    if (inherits(response, "error")) {
      message(strwrap(response,
                      initial = "", prefix = "\n"))
      return(NULL)
    }
    
    jsonResponse <- grepl("json", headers(response)$`content-type`)
    
    if (attemptCount > maxQueryAttempts) {
      message(strwrap("Exceeded max retry limit.", 
                      initial = "", prefix = "\n"))
      return(NULL)
    }
  }
  if(status_code(response) != 200) {
        message(paste("Final retry returned status code ",status_code(response)))
        return(NULL)
  }
  
  return(response)
}