library(shinymanager)

#### init the Sqlite Database
# Credentials data
credentials <- data.frame(
  user = Sys.getenv("user_manager"),
  password = Sys.getenv("password_manager"), # password will automatically be hashed
  admin = c(T),
  stringsAsFactors = FALSE
)

# Create the database
create_db(
  credentials_data = credentials,
  sqlite_path = "db/database.sqlite", # will be created
  # passphrase = key_get("obiwankenobi")
  passphrase = Sys.getenv("sql_db_pass")  # or just a word, without keyring
)
