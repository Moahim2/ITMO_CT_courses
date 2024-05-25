FROM liquibase/liquibase:4.16
COPY --chown=liquibase:liquibase liquibase.sh /liquibase/
RUN chmod +x /liquibase/liquibase.sh
COPY --chown=liquibase:liquibase changelog /liquibase/changelog
ENTRYPOINT ["/liquibase/liquibase.sh"]