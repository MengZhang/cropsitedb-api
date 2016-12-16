# Add obs_vars variables to metadata table

# --- !Ups
ALTER TABLE ace_metadata ADD obs_vars text;

# --- !Downs

ALTER TABLE ace_metadata DROP obs_vars;
