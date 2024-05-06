# DataBase schema for Kafka submodule.

## Table `export_type`

Table contains information about export types. 

### Columns:
`PK`: `code`

- `code`: `(String)` name of type;
- `description`: `(String)` definition of type;
- `retry_count`: `(int)` max count of attempts to processing data for sending to kafka;



## Table `export_task`

Table contains all tasks for sending data to kafka.

### Columns:
`PK`: `id`;

`FK (for export_type table)`: `type`.

- `id`: `(long)` simple id of task;
- `type`: `(String)` code of export_type;
- `status`: `(String)` may be 4 types
  - `NEW`: the task was just added to the database and has never been processed yet;
  - `PROCESSING`: the task started processing some (`< retry_count`) number of types;
  - `SUCCESS`: (task was completed) the data associated with the task was successfully sent to kafka;
  - `ERROR`: an uncorrectable error was committed during the execution of the task or it could not be processed in the allowed number of attempts.
- `string_key`: `(String)` key of data;
- `number_key`: `(long)` key of data;
- `retries_count`: `(int)` current count of attempts to processing;
- `updated_at`: `(TIMESTAMP)` last time of task's updated.

