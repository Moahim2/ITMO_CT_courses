# DataBase schema for DWH submodule.

## Table `agreement_info`

Table contains information about states of agreements.

Table has partitioning by business_date. 

### Columns:
`PK`: `(business_date, agreement_id, updatedAt)`;

- `agreement_id`: `(String)` id of agreement in format `L-<uuid>`;
- `code`: name of product with version;
- `client_id`: `(String)` id for bijection between client and agreement in format `CLI-<string>`;
- `loan_term`: `(int)` loan of term;
- `principal_amount`: `(BigDecimal)` principal amount;
- `interest`: `(BigDecimal)` interest (per year as a percentage);
- `origination_amount`: `(BigDecimal)` origination amount;
- `status`: `(String)` status of agreement;
- `disbursement_date`: `(Date)` disbursement date;
- `next_apyment_date`: `(Date)` the next last date for payment on loan;
- `overdue_balance`: `(BigDecimal)` amount in OVERDUE account;
- `main_balance`: `(BigDecimal)` amount in MAIN account;
- `created_at`: `(TIMESTAMP)` time of creation this agreement;
- `updated_at`: `(TIMESTAMP)` last time of updating this agreement;
- `business_date`: `(Date)` last day of updated this agreement;



## Table `application_info`

Table contains information about states of applications.

Table has partitioning by business_date.

### Columns:
`PK`: `(business_date, application_id, updatedAt)`;

- `application_id`: `(String)` id of application in format `APP-<uuid>`;
- `client_id`: `(String)` id of client in format `CLI-<uuid>`;
- `disbursement_amount`: `(int)` amount of loan;
- `status`: `(String)` status of application;
- `created_at`: `(TIMESTAMP)` time of creation this application;
- `updated_at`: `(TIMESTAMP)` last time of updating this application;
- `business_date`: `(Date)` last day of updated this application;

