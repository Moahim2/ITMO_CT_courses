# DataBase schema for PE submodule.

## Table `product`

Table contains information about loan products. 

### Columns:
`PK`: `code`

- `code`: id composed of name of product and version of product;
- `min_loan_term`: `(BigDecimal)` minimum of loan of term;
- `max_loan_term`: `(BigDecimal)` maximum of loan of term;
- `min_principal_amount`: `(BigDecimal)` minimum of principal amount;
- `max_principal_amount`: `(BigDecimal)` maximum of principal amount;
- `min_interest`: `(BigDecimal)` minimum of interest (per year as a percentage);
- `max_interest`: `(BigDecimal)` maximum of interest (per year as a percentage);
- `min_origination_amount`: `(BigDecimal)` minimum of origination amount;
- `max_origination_amount`: `(BigDecimal)` maximum of origination amount.


## Table `agreement`

Table contains information about amounts.

### Columns:
`PK`: `id`;

`FK (for product table)`: `product_code`.

- `id`: `(String)` uniq id for agreement in format `L-<uuid>`;
- `product_code`: name of product with version;
- `client_id`: `(String)` id for bijection between client and agreement in format `CLI-<string>`;
- `loan_term`: `(int)` loan of term;
- `principal_amount`: `(BigDecimal)` principal amount;
- `interest`: `(BigDecimal)` interest (per year as a percentage);
- `origination_amount`: `(BigDecimal)` origination amount;
- `status`: `(String)` may be 4 types
  - `ACTIVE`: if the bank disbursemented the money to the client under the agreement
              and payment_schedule was created;
  - `WAITING`: if agreement was created, but payment_schedule was created;
  - `REJECTED`: if agreement was stopped for some reason;
  - `CLOSED`: if agreement was closed because client disbursemented all money to bank;
- `disbursement_date`: `(Date)` disbursement date;
- `next_apyment_date`: `(Date)` the next last date for payment on loan.


## Table `payment_schedule`

Table contains information about payment schedule.

It is an intermediate link in the search for all payments for the agreement.

### Columns:
`PK`: `id`;

`FK (for agreement table)`: `agreement_id`.

- `id`: `(String)` uniq id for payment schedule in format `PS-<uuid>`;
- `agreement_id`: `(String)` id for agreement in format `L-<uuid>`;
- `version`: version of schedule.


## Table `payment_schedule_payment`

Table contains information about a specific payment according to a specific scheme.

Unique is the pair of `payment_schedule_id` and `payment_date`.

### Columns:
`PK`: (while unused in logic) `id`;

`FK (for payment_schedule)`: `payment_schedule_id`.

- `id`: `(Long)` uniq id for payment in format `BigSerial`;
- `payment_schedule_id`: `(String)` id for schedule in format `PS-<uuid>`;
- `status`: `(String)` may be 3 types
  - `FUTURE`: must be paid in the future (the deadline has not yet come out);
  - `PAID`: already paid payment;
  - `OVERDUE`: overdue payment;
- `payment_date`: `(Date)` last date for paid;
- `period_payment`: `(BigDecimal)` pmt (annual interest and paid every month);
- `interest_payment`: `(BigDecimal)` ipmt (annual interest and paid every month);
- `principal_payment`: `(BigDecimal)` ppmt (annual interest and paid every month).


