# DataBase schema for origination submodule.

## Table `client`

Table contains information about client's personal information.


### Columns:
`PK`: `id`

- `id`: `(String)` uniq id for agreement in format `CLI-<uuid>`;
- `first_name`: `(String)` first name of client;
- `last_name`: `(String)` last name of client;
- `email`: `(String)` email of client (`uniq` field);
- `salary`: `(int)` salary of client.


## Table `application`

Table contains information about applications.

### Columns:
`PK`: `id`;

`FK (for client table)`: `client_id`.

- `id`: `(String)` uniq id for application in format `APP-<uuid>`;
- `client_id`: `(String)` id of client in format `CLI-<uuid>`;
- `requested_disbursement_amount`: `(int)` amount of loan;
- `status`: `(String)` may be 5 types
  - `NEW`: must be scoring in future (just created);
  - `SCORING`: scoring (requested in scoring submodule);
  - `ACCEPTED`: scoring answered that application is correct;
  - `ACTIVE`: the client was paid money and was created schedule of payments;
  - `CLOSED`: the application was closed due to repayment or canceled.
