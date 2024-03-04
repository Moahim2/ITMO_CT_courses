CREATE TABLE payment_schedule(
    id VARCHAR NOT NULL PRIMARY KEY DEFAULT concat('PS-'::varchar, gen_random_uuid()::varchar),
    agreement_id VARCHAR NOT NULL REFERENCES agreement (id),
    version BIGINT NOT NULL
);

CREATE TABLE payment_schedule_payment(
    id BIGSERIAL PRIMARY KEY NOT NULL,
    payment_schedule_id VARCHAR NOT NULL REFERENCES payment_schedule (id),
    status VARCHAR NOT NULL check (status IN ('FUTURE', 'PAID', 'OVERDUE')),
    payment_date DATE NOT NULL,
    period_payment NUMERIC(20, 5) NOT NULL,
    interest_payment NUMERIC(20, 5) NOT NULL,
    principal_payment NUMERIC(20, 5) NOT NULL,
    period_number INT NOT NULL
);

