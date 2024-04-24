INSERT INTO export_type (
                         code,
                         description
)
VALUES ('APPLICATION', 'The default task is to upload the status of the application to kafka.'),
       ('AGREEMENT', 'The default task is to upload the status of the agreement to kafka.')
