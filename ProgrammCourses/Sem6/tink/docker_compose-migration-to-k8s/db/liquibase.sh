#!/bin/bash
set -e
echo "***** Execute main operation *****"
liquibase --headless=true --url="jdbc:postgresql://${DB_HOST}:${DB_PORT}/${DB_DATABASE}" --username=$DB_USER --password=$DB_PASSWORD --default-schema-name=$DB_SCHEMA "$@" --changelog-file=changelog/root-changelog.yml
echo "***** Operation completed *****"
if [[ x"${DB_TAG}" == "x" ]]; then
  echo "***** Setting tag is not required *****"
else
  echo "***** Set tag ${DB_TAG} *****"
  liquibase --headless=true --url="jdbc:postgresql://${DB_HOST}:${DB_PORT}/${DB_DATABASE}" --username=$DB_USER --password=$DB_PASSWORD --default-schema-name=$DB_SCHEMA tag --tag=$DB_TAG
fi
echo "***** Check db status *****"
liquibase --headless=true --url="jdbc:postgresql://${DB_HOST}:${DB_PORT}/${DB_DATABASE}" --changeLogFile=changelog/root-changelog.yml --username=$DB_USER --password=$DB_PASSWORD --default-schema-name=$DB_SCHEMA status