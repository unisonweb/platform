create table if not exists temp_entity_type_description (
  id integer primary key not null,
  description text unique not null
);
insert into temp_entity_type_description values
  (0, 'Term Component'),
  (1, 'Decl Component'),
  (2, 'Namespace'),
  (3, 'Patch'),
  (4, 'Causal')
  ON CONFLICT DO NOTHING
;

-- A "temp entity" is a term/decl/namespace/patch/causal that we cannot store in the database proper due to missing
-- dependencies.
--
-- The existence of each `temp_entity` row implies the existence of one or more corresponding
-- `temp_entity_missing_dependency` rows: it does not make sense to make a `temp_entity` row for a thing that has no
-- missing dependencies!
--
-- Similarly, each `temp_entity` row implies we do not have the entity in the database proper. When and if we *do* store
-- an entity proper (after storing all of its dependencies), we should always atomically delete the corresponding
-- `temp_entity` row, if any.
create table if not exists temp_entity (
  hash text primary key not null,
  blob bytes not null,
  type_id integer not null references temp_entity_type_description(id)
);

-- A many-to-many relationship between `temp_entity` (entities we can't yet store due to missing dependencies), and the
-- non-empty set of hashes of each entity's dependencies.
--
-- We store with each missing dependency the JWT that Unison Share provided us to download that dependency. For
-- downloading a particular dependency #bar, we only need one JWT, even if it's in the table multiple times. (In fact,
-- in this case, there is one "best" JWT - the one with the latest expiry time).
--
-- The JWTs are also encoded in the local ids part of entity itself (`temp_entity.blob`), but we don't want to have to
-- keep going back there there to decode during a pull.
--
-- For example, if we wanted to store term #foo, but couldn't because it depends on term #bar which we don't have yet,
-- we would end up with the following rows.
--
--  temp_entity
-- +------------------------+
-- | hash | blob | type_id  |
-- |========================|
-- | #foo | ...  | 0 (term) |
-- +------------------------+
--
--  temp_entity_missing_dependency
-- +----------------------------------------+
-- | dependent | dependency | dependencyJwt |
-- |========================================|
-- | #foo      | #bar       | aT.Eb.cx      |
-- +----------------------------------------+
create table if not exists temp_entity_missing_dependency (
  dependent text not null references temp_entity(hash),
  dependency text not null,
  dependencyJwt text not null,
  unique (dependent, dependency)
);
create index if not exists temp_entity_missing_dependency_ix_dependent on temp_entity_missing_dependency (dependent);
create index if not exists temp_entity_missing_dependency_ix_dependency on temp_entity_missing_dependency (dependency);
