-- Add a new column to the project_branch table to store the last time that project branch was accessed.
-- This column is stored as a unix epoch time.
ALTER TABLE project_branch ADD COLUMN last_accessed INTEGER NULL;
