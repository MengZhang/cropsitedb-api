# Add BATCH support, extend climate metadata, add generating tool support, extend simulated data

# --- !Ups
ALTER TABLE acmo_metadata ADD batch_dome text;
ALTER TABLE acmo_metadata ADD batch_runNUM text;
ALTER TABLE acmo_metadata ADD clim_cat text;
ALTER TABLE acmo_metadata ADD cmss text;
ALTER TABLE acmo_metadata ADD bdid text;
ALTER TABLE acmo_metadata ADD tool_version text;
ALTER TABLE acmo_metadata ADD sraa_s text;
ALTER TABLE acmo_metadata ADD tmaxa_s text;
ALTER TABLE acmo_metadata ADD tmina_s text;
ALTER TABLE acmo_metadata ADD tavga_s text;
ALTER TABLE acmo_metadata ADD co2d_s text;

# --- !Downs
ALTER TABLE acmo_metadata DROP batch_dome;
ALTER TABLE acmo_metadata DROP batch_runNUM;
ALTER TABLE acmo_metadata DROP clim_cat;
ALTER TABLE acmo_metadata DROP cmss;
ALTER TABLE acmo_metadata DROP bdid;
ALTER TABLE acmo_metadata DROP tool_version;
ALTER TABLE acmo_metadata DROP sraa_s;
ALTER TABLE acmo_metadata DROP tmaxa_s;
ALTER TABLE acmo_metadata DROP tmina_s;
ALTER TABLE acmo_metadata DROP tavga_s;
ALTER TABLE acmo_metadata DROP co2d_s;
