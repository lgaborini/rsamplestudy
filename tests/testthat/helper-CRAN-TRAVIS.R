
# Grab environment variables
TRAVIS <- identical(tolower(Sys.getenv("TRAVIS")), "true")
NOT_CRAN <- identical(Sys.getenv("NOT_CRAN"), "true")
