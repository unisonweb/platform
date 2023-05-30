-- v3 codebase schema

CREATE TABLE schema_version (
  version INTEGER NOT NULL
);

-- actually stores the 512-byte hashes
CREATE TABLE hash (
  id INTEGER PRIMARY KEY NOT NULL,
  -- this would be the full hash, represented as base32 instead of bytes,
  -- to optimize for looking them up by prefix.
  base32 TEXT NOT NULL
);
CREATE UNIQUE INDEX hash_base32 ON hash(base32 COLLATE NOCASE);
-- Per https://sqlite.org/optoverview.html#the_like_optimization,
-- we need COLLATE NOCASE to enable prefix scanning with `LIKE`.
-- If we want LIKE to be case sensitive (defaults to no) then
-- see that link.
-- We want:
    -- sqlite> explain query plan select id from hash where base32 like 'a1b2c3%'
    -- QUERY PLAN
    -- `--SEARCH TABLE hash USING COVERING INDEX hash_base32 (base32>? AND base32<?)
-- Not:
    -- sqlite> explain query plan select id from hash where base32 like 'a1b2c3%'
    -- QUERY PLAN
    -- `--SCAN TABLE hash

CREATE TABLE text (
  id INTEGER PRIMARY KEY NOT NULL,
  text TEXT UNIQUE NOT NULL
);

-- The `hash_object` table is a layer of indirection that allows multiple
-- hashes to be associated to the same object. For example, if the hashing
-- algorithm for the object is changed.
-- I could imagine a UNIQUE (object_id, hash_version) constraint
-- or a UNIQUE (hash_id, hash_version) constraint, or both, but I'm not sure
-- if that will cause trouble later?
-- So: SELECT object.id, bytes FROM object
--      INNER JOIN hash_object ON object_id = object.id
--      INNER JOIN hash ON hash_id = hash.id
--     WHERE base32 LIKE 'a1b2c3%'
CREATE TABLE hash_object (
  -- hashes are UNIQUE, many hashes correspond to one object
  -- (causal nodes are not considered objects atm)
  hash_id INTEGER PRIMARY KEY NOT NULL CONSTRAINT hash_object_fk1 REFERENCES hash(id),
  object_id INTEGER NOT NULL CONSTRAINT hash_object_fk2 REFERENCES object(id),
  hash_version INTEGER NOT NULL
);
-- efficient lookup of hashes by objects
CREATE INDEX hash_object_object_id ON hash_object(object_id);

-- This table is just for diagnostic queries, not for normal ucm operation,
-- by joining `ON object.type_id = object_type_description.id`
CREATE TABLE object_type_description (
  id INTEGER PRIMARY KEY NOT NULL,
  description TEXT UNIQUE NOT NULL
);
INSERT INTO object_type_description (id, description) VALUES
    (0, "Term Component"), -- ping x = pong (x + 1), pong x = ping (x + 1)
    (1, "Decl Component"), -- unique type Animal = Cat | Dog | Mouse
    (2, "Namespace"), -- a one-level slice with no history
    (3, "Patch") -- replace term #abc with term #def
    ;

-- `object` stores binary blobs that are uniquely identified by hash (alone).
-- Within the database, objects (including terms, decls, namespaces) are primarily
-- referenced by their object_ids, not hash ids.  (The use of object_id is
-- meant to prove that the referenced object actually exists in the database,
-- whereas if hash_ids were used, we could only prove that the hash was in the
-- database.)
-- The `hash_object` table allows us to associate multiple hashes to objects,
-- but when we want to display a hash back to the user, we have to choose one.
-- The `primary_hash_id` tells us which one we'll use.
CREATE TABLE object (
  id INTEGER PRIMARY KEY NOT NULL,
  primary_hash_id INTEGER NOT NULL CONSTRAINT object_fk1 REFERENCES hash(id),
  type_id INTEGER NOT NULL CONSTRAINT object_fk2 REFERENCES object_type_description(id),
  bytes BLOB NOT NULL
);
-- look up objects by primary_hash_id.
CREATE UNIQUE INDEX object_hash_id ON object(primary_hash_id);
-- filter objects by
CREATE INDEX object_type_id ON object(type_id);

-- `causal` references value hash ids instead of value ids, in case you want
-- to be able to drop values and keep just the causal spine.
-- `commit_flag` and `gc_generation` are basically unused at the moment.
CREATE TABLE causal (
  self_hash_id INTEGER PRIMARY KEY NOT NULL CONSTRAINT causal_fk1 REFERENCES hash(id),
  value_hash_id INTEGER NOT NULL CONSTRAINT causal_fk2 REFERENCES hash(id)
);
-- Don't remember why this index exists —AI
CREATE INDEX causal_value_hash_id ON causal(value_hash_id);

-- We expect exactly 1 row, which we overwrite when we setRootNamespace.
CREATE TABLE namespace_root (
  causal_id INTEGER PRIMARY KEY NOT NULL CONSTRAINT namespace_root_fk1 REFERENCES causal(self_hash_id)
);

-- LCA computations only need to look at this table
-- A causal can have many parents, and a parent may be a parent to many causals.
CREATE TABLE causal_parent (
  causal_id INTEGER NOT NULL CONSTRAINT causal_parent_fk1 REFERENCES causal(self_hash_id),
  parent_id INTEGER NOT NULL CONSTRAINT causal_parent_fk2 REFERENCES causal(self_hash_id),
  PRIMARY KEY (causal_id, parent_id)
) WITHOUT ROWID;
CREATE INDEX causal_parent_causal_id ON causal_parent(causal_id); -- maybe redundant? covered by PK index? —AI
CREATE INDEX causal_parent_parent_id ON causal_parent(parent_id); -- ?  Potentially allow a "redo" command?

-- links reference.id to causals
-- Currently unused.
CREATE TABLE causal_metadata (
  causal_id INTEGER NOT NULL REFERENCES causal(self_hash_id),
  metadata_object_id INTEGER NOT NULL REFERENCES object(id),
  metadata_component_index INTEGER NOT NULL,
  PRIMARY KEY (causal_id, metadata_object_id, metadata_component_index)
) WITHOUT ROWID;
CREATE INDEX causal_metadata_causal_id ON causal_metadata(causal_id);

CREATE TABLE watch_result (
  -- See Note [Watch expression identifier]
  hash_id INTEGER NOT NULL CONSTRAINT watch_result_fk1 REFERENCES hash(id),
  component_index INTEGER NOT NULL,

  result BLOB NOT NULL, -- evaluated result of the watch expression
  PRIMARY KEY (hash_id, component_index)
) WITHOUT ROWID;


CREATE TABLE watch (
  -- See Note [Watch expression identifier]
  hash_id INTEGER NOT NULL CONSTRAINT watch_fk1 REFERENCES hash(id),
  component_index INTEGER NOT NULL,

  watch_kind_id INTEGER NOT NULL CONSTRAINT watch_fk2 REFERENCES watch_kind_description(id),
  PRIMARY KEY (hash_id, component_index, watch_kind_id)
) WITHOUT ROWID;
CREATE INDEX watch_kind ON watch(watch_kind_id);

-- Note [Watch expression identifier]
-- The hash_id + component_index is an unevaluated term reference. We use hash_id instead of object_id because the
-- unevaluated term may not exist in the codebase: it is not added merely by watching it without a name, e.g `> 2 + 3`.


CREATE TABLE watch_kind_description (
  id INTEGER PRIMARY KEY NOT NULL,
  description TEXT UNIQUE NOT NULL
);
INSERT INTO watch_kind_description (id, description) VALUES
  (0, "Regular"), -- won't be synced
  (1, "Test") -- will be synced
  ;

-- Related to the discussion at the `object` table, `find_type_index` indexes
-- the types by hash-based references instead of object-based references, because
-- they may be arbitrary types, not just the head types that are stored in the
-- codebase.  The terms having these types are indexed by object-based referents.
CREATE TABLE find_type_index (
  type_reference_builtin INTEGER NULL CONSTRAINT find_type_index_fk1 REFERENCES text(id),
  type_reference_hash_id INTEGER NULL CONSTRAINT find_type_index_fk2 REFERENCES hash(id),
  type_reference_component_index INTEGER NULL,
  term_referent_object_id INTEGER NOT NULL CONSTRAINT find_type_index_fk3 REFERENCES object(id),
  term_referent_component_index INTEGER NOT NULL,
  term_referent_constructor_index INTEGER NULL,
  CONSTRAINT find_type_index_c1 UNIQUE (
    term_referent_object_id,
    term_referent_component_index,
    term_referent_constructor_index
  ),
  CONSTRAINT find_type_index_c2 CHECK (
    (type_reference_builtin IS NULL) =
    (type_reference_hash_id IS NOT NULL)
  ),
  CONSTRAINT find_type_index_c3 CHECK (
    (type_reference_hash_id IS NULL) =
    (type_reference_component_index IS NULL)
  )
);
CREATE INDEX find_type_index_type ON find_type_index (
    type_reference_builtin,
    type_reference_hash_id,
    type_reference_component_index
);

CREATE TABLE find_type_mentions_index (
  type_reference_builtin INTEGER NULL CONSTRAINT find_type_mentions_index_fk1 REFERENCES text(id),
  type_reference_hash_id INTEGER NULL CONSTRAINT find_type_mentions_index_fk2 REFERENCES hash(id),
  type_reference_component_index INTEGER NULL,
  term_referent_object_id INTEGER NOT NULL CONSTRAINT find_type_mentions_index_fk3 REFERENCES object(id),
  term_referent_component_index INTEGER NOT NULL,
  term_referent_constructor_index INTEGER NULL,
  CONSTRAINT find_type_mentions_index_c1 CHECK (
    (type_reference_builtin IS NULL) =
    (type_reference_hash_id IS NOT NULL)
  ),
  CONSTRAINT find_type_mentions_index_c2 CHECK (
    (type_reference_hash_id IS NULL) =
    (type_reference_component_index IS NULL)
  )
);
CREATE INDEX find_type_mentions_index_type ON find_type_mentions_index (
  type_reference_builtin,
  type_reference_hash_id,
  type_reference_component_index
);

-- dependents and dependencies are all in the codebase, so they use object-based references.
CREATE TABLE dependents_index (
  dependency_builtin INTEGER NULL CONSTRAINT dependents_index_fk1 REFERENCES text(id),
  dependency_object_id INTEGER NULL CONSTRAINT dependents_index_fk2 REFERENCES object(id),
  dependency_component_index INTEGER NULL,
  dependent_object_id INTEGER NOT NULL CONSTRAINT dependents_index_fk3 REFERENCES object(id),
  dependent_component_index INTEGER NOT NULL,
  CONSTRAINT dependents_index_c1 CHECK (
    (dependency_builtin IS NULL) =
    (dependency_object_id IS NOT NULL)
  ),
  CONSTRAINT dependents_index_c2 CHECK (
    (dependency_object_id IS NULL) =
    (dependency_component_index IS NULL)
  )
);
CREATE INDEX dependents_by_dependency ON dependents_index (
  dependency_builtin,
  dependency_object_id,
  dependency_component_index
);
CREATE INDEX dependencies_by_dependent ON dependents_index (
  dependent_object_id,
  dependent_component_index
);


-- This table allows us to look up which branch hashes have a name lookup.
CREATE TABLE name_lookups (
    root_branch_hash_id INTEGER PRIMARY KEY REFERENCES hash(id) ON DELETE CASCADE
);

CREATE TABLE scoped_term_name_lookup (
  root_branch_hash_id INTEGER NOT NULL REFERENCES hash(id) ON DELETE CASCADE,

  -- The name of the term in reversed form, with a trailing '.':
  -- E.g. map.List.base.
  --
  -- The trailing '.' is helpful when performing suffix queries where we may not know
  -- whether the suffix is complete or not, e.g. we could suffix search using any of the
  -- following globs and it would still find 'map.List.base.':
  --  map.List.base.*
  --  map.List.*
  --  map.*
  reversed_name TEXT NOT NULL,

  -- The last name segment of the name. This is used when looking up names for
  -- suffixification when building PPEs.
  -- E.g. for the name 'base.List.map' this would be 'map'
  last_name_segment TEXT NOT NULL,

  -- The namespace containing this definition, not reversed, with a trailing '.'
  -- The trailing '.' simplifies GLOB queries, so that 'base.*' matches both things in
  -- 'base' and 'base.List', but not 'base1', which allows us to avoid an OR in our where
  -- clauses which in turn helps the sqlite query planner use indexes more effectively.
  --
  -- example value: 'base.List.'
  namespace TEXT NOT NULL,
  referent_builtin TEXT NULL,
  referent_component_hash TEXT NULL,
  referent_component_index INTEGER NULL,
  referent_constructor_index INTEGER NULL,
  referent_constructor_type INTEGER NULL,
  PRIMARY KEY (root_branch_hash_id, reversed_name, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index)
);

-- This index allows finding all names we need to consider within a given namespace for
-- suffixification of a name.
-- It may seem strange to use last_name_segment rather than a suffix search over reversed_name name her
-- but SQLite will only optimize for a single prefix-glob at once, so we can't glob search
-- over both namespace and reversed_name, but we can EXACT match on last_name_segment and
-- then glob search on the namespace prefix, and have SQLite do the final glob search on
-- reversed_name over rows with a matching last segment without using an index and should be plenty fast.
CREATE INDEX scoped_term_names_by_namespace_and_last_name_segment ON scoped_term_name_lookup(root_branch_hash_id, last_name_segment, namespace);

-- This index allows us to find all names with a given ref within a specific namespace
CREATE INDEX scoped_term_name_by_referent_lookup ON scoped_term_name_lookup(root_branch_hash_id, referent_builtin, referent_component_hash, referent_component_index, referent_constructor_index, namespace);

-- Allows fetching ALL names within a specific namespace prefix. We currently use this to
-- pretty-print on share, but will be replaced with a more precise set of queries soon.
CREATE INDEX scoped_term_names_by_namespace ON scoped_term_name_lookup(root_branch_hash_id, namespace);


CREATE TABLE scoped_type_name_lookup (
  root_branch_hash_id INTEGER NOT NULL REFERENCES hash(id),
  -- The name of the term: E.g. List.base
  reversed_name TEXT NOT NULL,
  -- The last name segment of the name. This is used when looking up names for
  -- suffixification when building PPEs.
  -- E.g. for the name 'base.List.map' this would be 'map'
  last_name_segment TEXT NOT NULL,
  -- The namespace containing this definition, not reversed, with a trailing '.'
  -- The trailing '.' simplifies GLOB queries, so that 'base.*' matches both things in
  -- 'base' and 'base.List', but not 'base1', which allows us to avoid an OR in our where
  -- clauses which in turn helps the sqlite query planner use indexes more effectively.
  --
  -- example value: 'base.List.'
  namespace TEXT NOT NULL,
  reference_builtin TEXT NULL,
  reference_component_hash INTEGER NULL,
  reference_component_index INTEGER NULL,
  PRIMARY KEY (reversed_name, reference_builtin, reference_component_hash, reference_component_index)
);

-- This index allows finding all names we need to consider within a given namespace for
-- suffixification of a name.
-- It may seem strange to use last_name_segment rather than a suffix search over reversed_name name here
-- but SQLite will only optimize for a single prefix-glob at once, so we can't glob search
-- over both namespace and reversed_name, but we can EXACT match on last_name_segment and
-- then glob search on the namespace prefix, and have SQLite do the final glob search on
-- reversed_name over rows with a matching last segment without using an index and should be plenty fast.
CREATE INDEX scoped_type_names_by_namespace_and_last_name_segment ON scoped_type_name_lookup(root_branch_hash_id, last_name_segment, namespace);


-- This index allows us to find all names with a given ref within a specific namespace.
CREATE INDEX scoped_type_name_by_reference_lookup ON scoped_type_name_lookup(root_branch_hash_id, reference_builtin, reference_component_hash, reference_component_index, namespace);


-- Allows fetching ALL names within a specific namespace prefix. We currently use this to
-- pretty-print on share, but will be replaced with a more precise set of queries soon.
CREATE INDEX scoped_type_names_by_namespace ON scoped_type_name_lookup(root_branch_hash_id, namespace);
