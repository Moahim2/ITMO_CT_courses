CREATE TABLE product (
    code VARCHAR PRIMARY KEY NOT NULL,
    min_loan_term INT NOT NULL,
    max_loan_term INT NOT NULL,
    min_principal_amount NUMERIC(20, 5) NOT NULL,
    max_principal_amount NUMERIC(20, 5) NOT NULL,
    min_interest NUMERIC(20, 5) NOT NULL,
    max_interest NUMERIC(20, 5) NOT NULL,
    min_origination_amount NUMERIC(20, 5) NOT NULL,
    max_origination_amount NUMERIC(20, 5) NOT NULL
);
