CREATE TABLE account(
    agreement_id VARCHAR NOT NULL REFERENCES agreement (id),
    type VARCHAR NOT NULL check (type IN ('MAIN', 'OVERDUE')),
    balance NUMERIC(20, 5) NOT NULL,
    PRIMARY KEY (agreement_id, type)
);