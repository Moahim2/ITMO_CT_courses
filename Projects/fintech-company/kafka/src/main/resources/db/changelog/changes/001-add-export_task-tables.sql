CREATE TABLE export_type
(
    code        VARCHAR,
    description VARCHAR NOT NULL,
    retry_count INT     NOT NULL DEFAULT 3,

    PRIMARY KEY (code)
);

CREATE SEQUENCE s_export_task;

CREATE TABLE export_task
(
    id            BIGINT PRIMARY KEY                DEFAULT nextval('s_export_task'),
    type          VARCHAR                  NOT NULL,
    status        VARCHAR                  NOT NULL,
    string_key    VARCHAR,
    number_key    BIGINT,
    retries_count INT                      NOT NULL DEFAULT 0,
    updated_at    TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp(),

    CONSTRAINT export_tasks_fk1 FOREIGN KEY (type) REFERENCES export_type (code),
    CONSTRAINT export_tasks_ch1 CHECK (status in ('NEW', 'PROCESSING', 'SUCCESS', 'ERROR'))
);
