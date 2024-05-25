--liquibase formatted sql

--changeset author:a.sulyz failOnError:true labels:TASK-1
create sequence agreement_seq;

create table agreement (
    id     bigint,
    status varchar,

    primary key (id)
);

-----

-- rollback drop sequence agreement_seq;
-- rollback drop table agreement;