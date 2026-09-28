library(glue)

project_id <- Sys.getenv("project_id")

list.files(glue("/home/airflow/dags/{project_id}"))

source(glue("/home/airflow/dags/{project_id}/email/milestone_snapshots.R"))