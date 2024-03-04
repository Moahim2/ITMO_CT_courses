CREATE TABLE agreement (
    id VARCHAR NOT NULL PRIMARY KEY DEFAULT concat('L-'::varchar, gen_random_uuid()::varchar),
    product_code varchar NOT NULL REFERENCES product (code),
    client_id VARCHAR NOT NULL,
    loan_term INT NOT NULL,
    principal_amount NUMERIC(20, 5) NOT NULL,
    interest NUMERIC(20, 5) NOT NULL,
    origination_amount NUMERIC(20, 5) NOT NULL,
    status VARCHAR NOT NULL check (status IN ('ACTIVE', 'WAITING', 'REJECTED', 'CLOSED')),
    disbursement_date DATE,
    next_payment_date DATE
);
