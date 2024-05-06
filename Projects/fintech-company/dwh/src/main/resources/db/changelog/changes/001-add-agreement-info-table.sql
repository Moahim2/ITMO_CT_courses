CREATE SEQUENCE s_agreement_info;

CREATE TABLE agreement_info
(
    agreement_id       VARCHAR        NOT NULL,
    code               VARCHAR        NOT NULL,
    client_id          VARCHAR        NOT NULL,
    loan_term          INT            NOT NULL,
    principal_amount   NUMERIC(20, 5) NOT NULL,
    interest           NUMERIC(20, 5) NOT NULL,
    origination_amount NUMERIC(20, 5) NOT NULL,
    status             VARCHAR        NOT NULL,
    disbursement_date  DATE,
    next_payment_date  DATE,
    overdue_balance    NUMERIC(20, 5),
    main_balance       NUMERIC(20, 5),
    created_at         TIMESTAMP      NOT NULL,
    updated_at         TIMESTAMP      NOT NULL,
    business_date      DATE           NOT NULL,
    PRIMARY KEY (business_date, agreement_id, updated_at)
) PARTITION BY RANGE (business_date);

CREATE TABLE agreement_y2024 PARTITION OF agreement_info
    FOR VALUES FROM ('2024-01-01') TO ('2025-01-01');

CREATE INDEX ON agreement_info (business_date);
