##################################################################
##                      Upload Table to S3                      ##
##################################################################

upload_df_to_aws_s3 <- function(df){
  svc <- s3(
    config = list(
      credentials = list(
        creds = list(
          access_key_id = keyring::key_get("aws_keys", "AWS_ACCESS_KEY_ID"),
          secret_access_key = keyring::key_get("aws_keys", "AWS_SECRET_ACCESS_KEY")
        )
      ),
      region = keyring::key_get("aws_keys", "AWS_REGION")
    )
  )
  
  write_csv(df, "current_stars_ranking.csv")
  
  svc$put_object(
    Body = "current_stars_ranking.csv",
    Bucket = "avantgarde-stars",
    Key = "current_stars_ranking.csv",
    Metadata = list(
      report_reference_date = Sys.Date()
    )
  )
  
  file.remove("current_stars_ranking.csv")
}

