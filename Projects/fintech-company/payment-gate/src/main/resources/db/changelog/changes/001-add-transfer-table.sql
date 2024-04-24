CREATE TABLE transfer (
    id VARCHAR NOT NULL PRIMARY KEY,
    agreement_id VARCHAR NOT NULL,
    status VARCHAR NOT NULL check (status IN ('EXECUTING', 'COMPLETED', 'REJECTED'))
);
