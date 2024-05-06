CREATE SEQUENCE s_application_info;

CREATE TABLE application_info
(
    application_id      VARCHAR   NOT NULL,
    client_id           VARCHAR   NOT NULL,
    disbursement_amount INT       NOT NULL,
    status              VARCHAR   NOT NULL,
    created_at          TIMESTAMP NOT NULL,
    updated_at          TIMESTAMP NOT NULL,
    business_date       DATE      NOT NULL,
    PRIMARY KEY (business_date, application_id, updated_at)
) PARTITION BY RANGE (business_date);

CREATE TABLE application_y2024 PARTITION OF application_info
    FOR VALUES FROM ('2024-01-01') TO ('2025-01-01');

CREATE INDEX ON application_info (business_date);