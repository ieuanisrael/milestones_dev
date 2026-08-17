install.packages("Microsoft365R")
library(Microsoft365R)

outlook <- get_business_outlook()

outlook$send_email(
  to = "recipient@example.com",
  subject = "Test Email",
  body = "Sent from R via Microsoft Graph"
)


