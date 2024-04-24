# DataBase schema for PG submodule.

## Table `transfer`

Table contains information about transfers (the transfer process is modeled in merchant-provider).

### Columns:
`PK`: `id`

- `id`: `(String)` id of transfer (received from mp) in format `tr<long>`;
- `agreement_id`: `(String)` id of agreement (from pe) in format `L-<uuid>`;
- `status`: `(String)` may be 3 types:
  - `EXECUTING`: when transfer has already sent to mp but information about its completion has not yet been received.
  - `COMPLETED`: when transfer has already completed and information about its has already received.
  - `REJECTED`: an additional flag indicating a transfer that did not end in success.
