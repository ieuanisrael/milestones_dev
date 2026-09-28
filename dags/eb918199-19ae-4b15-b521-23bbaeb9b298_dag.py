######################################################
#
#              MILESTONES_DEV
#
#              Author: ieuan.israel@cricketnsw.com.au
#
#              Created on Ludis Analytics
#
#              Date: 2026-9-28
#
######################################################


# For more information
# https://ludisanalytics.github.io/documentation/#/deploy_workflows

# Imports

from airflow import DAG
from LudisDagLibrary import LudisDagUtil
from airflow.operators.bash import BashOperator
from airflow.operators.email import EmailOperator

utils = LudisDagUtil('6049e734-a134-4218-939e-4b27416addc0')
variables = utils.airflow_variables
default_args = utils.default_args

schedule = variables["LUDIS_SCHEDULE_IDeb918199"]
if schedule.lower().strip() == 'none':
    schedule = None

dag = DAG(
    dag_id='eb918199-19ae-4b15-b521-23bbaeb9b298',
    default_args=default_args,
    description='An example DAG',
    schedule_interval=schedule
)

with dag:
    # Running a python script
    # python_task = utils.LudisPyOperator(filename = 'example.py', image = 'ludis-py')
    
    r_task = utils.LudisROperator(filename = 'run_email.r', image='ludis-r', installScript='install.sh' )
    
    bash_task = BashOperator(
        task_id='bash_task',
        bash_command='echo "Hello, World!"',
        dag=dag,
    )

    bash_task