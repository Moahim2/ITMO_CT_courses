CREATE TABLE client (
    id VARCHAR NOT NULL PRIMARY KEY DEFAULT concat('CLI-'::varchar, gen_random_uuid()::varchar),
    first_name VARCHAR NOT NULL,
    last_name VARCHAR NOT NULL,
    email VARCHAR NOT NULL UNIQUE,
    salary INT
);

CREATE TABLE application (
    id VARCHAR NOT NULL PRIMARY KEY DEFAULT concat('APP-'::varchar, gen_random_uuid()::varchar),
    client_id VARCHAR NOT NULL REFERENCES client (id),
    requested_disbursement_amount INT NOT NULL,
    status VARCHAR NOT NULL check (status IN ('NEW', 'SCORING', 'ACCEPTED', 'ACTIVE', 'CLOSED'))
);
