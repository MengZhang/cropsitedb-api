# Add support for upload progress

# --- !Ups

ALTER TABLE ace_datasets ADD COLUMN state text;

# --- !Downs

ALTER TABLE ace_datasets DROP COLUMN state;
