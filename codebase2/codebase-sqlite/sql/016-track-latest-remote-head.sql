-- Add a field for tracking the latest known causal hash for each remote project branch.
-- It's helpful for when we need to tell Share how much we know about a branch.

ALTER TABLE remote_project
  -- Note that there isn't a guarantee this hash has actually been synced into the codebase.
  ADD COLUMN last_known_causal_hash INTEGER NULL REFERENCES hash(id)
    ON DELETE SET NULL;
