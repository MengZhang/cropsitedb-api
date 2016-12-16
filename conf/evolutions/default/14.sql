# Add epcp_s and escp_s to acmo

# --- !Ups
ALTER TABLE acmo_metadata ADD epcp_s text;
ALTER TABLE acmo_metadata ADD escp_s text;

# --- !Downs

ALTER TABLE acmo_metadata DROP epcp_s;
ALTER TABLE acmo_metadata DROP escp_s;
