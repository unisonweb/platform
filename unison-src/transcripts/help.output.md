# Shows `help` output

``` ucm
scratch/main> help

  add
  `add` adds to the codebase all the definitions from the most recently typechecked file.
  
  add.preview
  `add.preview` previews additions to the codebase from the most recently typechecked file. This command only displays cached typechecking results. Use `load` to reparse & typecheck the file if the context has changed.
  
  add.run
  `add.run name` adds to the codebase the result of the most recent `run` command as `name`.
  
  alias.many (or copy)
  `alias.many <relative1> [relative2...] <namespace>` creates
  aliases `relative1`, `relative2`, ... in the namespace
  `namespace`.
  `alias.many foo.foo bar.bar .quux` creates aliases
  `.quux.foo.foo` and `.quux.bar.bar`.
  
  alias.term
  `alias.term foo bar` introduces `bar` with the same definition as `foo`.
  
  alias.type
  `alias.type Foo Bar` introduces `Bar` with the same definition as `Foo`.
  
  api
  `api` provides details about the API.
  
  auth.login
  Obtain an authentication session with Unison Share.
  `auth.login`authenticates ucm with Unison Share.
  
  back (or popd)
  `back`  undoes the last `switch` command.
  
  branch (or branch.create, create.branch)
  `branch foo`       forks the current project branch to a new
                     branch `foo`
  `branch /bar foo`  forks the branch `bar` of the current
                     project to a new branch `foo`
  
  branch.empty (or branch.create-empty, create.empty-branch)
  Create a new empty branch.
  
  branch.rename (or rename.branch)
  `branch.rename foo`  renames the current branch to `foo`
  
  branches (or list.branch, ls.branch, branch.list)
  `branches`      lists all branches in the current project
  `branches foo`  lists all branches in the project `foo`
  
  clear
  `clear`  Clears the screen.
  
  clone
  `clone @unison/json/topic json/my-topic`  creates
                                            `json/my-topic` from
                                            the remote branch
                                            `@unison/json/topic`
  `clone @unison/base base/`                creates `base/main`
                                            from the remote
                                            branch
                                            `@unison/base/main`
  `clone @unison/base /main2`               creates the branch
                                            `main2` in the
                                            current project from
                                            the remote branch
                                            `@unison/base/main`
  `clone /main /main2`                      creates the branch
                                            `main2` in the
                                            current project from
                                            the remote branch
                                            `main` of the
                                            current project's
                                            associated remote
                                            (see
                                            `help-topics remotes`)
  `clone /main my-fork/`                    creates
                                            `my-fork/main` from
                                            the branch `main` of
                                            the current
                                            project's associated
                                            remote (see
                                            `help-topics remotes`)
  
  compile (or compile.output)
  `compile main file`  Outputs a stand alone file that can be
                       directly loaded and executed by unison.
                       Said execution will have the effect of
                       running `!main`.
  
  create.author
  `create.author alicecoder "Alice McGee"` creates `alicecoder`
  values in `metadata.authors` and `metadata.copyrightHolders.`
  
  debug.clear-cache
  Clear the watch expression cache
  
  debug.doc-to-markdown
  `debug.doc-to-markdown term.doc`  Render a doc to markdown.
  
  debug.doctor
  Analyze your codebase for errors and inconsistencies.
  
  debug.dump-namespace
  Dump the namespace to a text file
  
  debug.dump-namespace-simple
  Dump the namespace to a text file
  
  debug.file
  View details about the most recent successfully typechecked file.
  
  debug.find.global
  `find`                           lists all definitions in the
                                   current namespace.
  `find foo`                       lists all definitions with a
                                   name similar to 'foo' in the
                                   current namespace (excluding
                                   those under 'lib').
  `find foo bar`                   lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the current
                                   namespace (excluding those
                                   under 'lib').
  `find-in namespace`              lists all definitions in the
                                   specified subnamespace.
  `find-in namespace foo bar`      lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the specified
                                   subnamespace.
  find.all foo                     lists all definitions with a
                                   name similar to 'foo' in the
                                   current namespace (including
                                   one level of 'lib').
  `find-in.all namespace`          lists all definitions in the
                                   specified subnamespace
                                   (including one level of its
                                   'lib').
  `find-in.all namespace foo bar`  lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the specified
                                   subnamespace (including one
                                   level of its 'lib').
  debug.find.global foo            Iteratively searches all
                                   projects and branches and
                                   lists all definitions with a
                                   name similar to 'foo'. Note
                                   that this is a very slow
                                   operation.
  
  debug.names.global
  `debug.names.global foo` Iteratively search across all
  projects and branches for names matching `foo`. Note that this
  is expected to be quite slow and is primarily for debugging
  issues with your codebase.
  
  debug.numberedArgs
  Dump the contents of the numbered args state.
  
  delete
  `delete foo` removes the term or type name `foo` from the namespace.                
  `delete foo bar` removes the term or type name `foo` and `bar` from the namespace.  
  
  delete.branch (or branch.delete)
  `delete.branch foo/bar`  deletes the branch `bar` in the
                           project `foo`
  `delete.branch /bar`     deletes the branch `bar` in the
                           current project
  
  delete.namespace
  `delete.namespace <foo>` deletes the namespace `foo`
  
  delete.namespace.force
  `delete.namespace.force <foo>` deletes the namespace `foo`,deletion will proceed even if other code depends on definitions in foo.
  
  delete.project (or project.delete)
  `delete.project foo`  deletes the local project `foo`
  
  delete.term
  `delete.term foo` removes the term name `foo` from the namespace.                
  `delete.term foo bar` removes the term name `foo` and `bar` from the namespace.  
  
  delete.term.verbose
  `delete.term.verbose foo` removes the term name `foo` from the namespace.                
  `delete.term.verbose foo bar` removes the term name `foo` and `bar` from the namespace.  
  
  delete.type
  `delete.type foo` removes the type name `foo` from the namespace.                
  `delete.type foo bar` removes the type name `foo` and `bar` from the namespace.  
  
  delete.type.verbose
  `delete.type.verbose foo` removes the type name `foo` from the namespace.                
  `delete.type.verbose foo bar` removes the type name `foo` and `bar` from the namespace.  
  
  delete.verbose
  `delete.verbose foo` removes the term or type name `foo` from the namespace.                
  `delete.verbose foo bar` removes the term or type name `foo` and `bar` from the namespace.  
  
  dependencies
  List the dependencies of the specified definition.
  
  dependents
  List the named dependents of the specified definition.
  
  deprecated.cd (or deprecated.namespace)
  Moves your perspective to a different namespace. Deprecated for now because too many important things depend on your perspective selection.
  
  `deprecated.cd foo.bar`   descends into foo.bar from the
                            current namespace.
  `deprecated.cd .cat.dog`  sets the current namespace to the
                            absolute namespace .cat.dog.
  `deprecated.cd ..`        moves to the parent of the current
                            namespace. E.g. moves from
                            '.cat.dog' to '.cat'
  `deprecated.cd`           invokes a search to select which
                            namespace to move to, which requires
                            that `fzf` can be found within your
                            PATH.
  
  deprecated.root-reflog
  `deprecated.root-reflog` lists the changes that have affected the root namespace. This has been deprecated in favor of `reflog` which shows the reflog for the current project.
  
  diff.namespace
  `diff.namespace before after` shows how the namespace `after`
                                differs from the namespace
                                `before`
  `diff.namespace before`       shows how the current namespace
                                differs from the namespace
                                `before`
  
  display
  `display foo` prints a rendered version of the term `foo`.
  `display` without arguments invokes a search to select a definition to display, which requires that `fzf` can be found within your PATH.
  
  display.to
  `display.to <filename> foo` prints a rendered version of the
  term `foo` to the given file.
  
  docs
  `docs foo` shows documentation for the definition `foo`.
  `docs` without arguments invokes a search to select which definition to view documentation for, which requires that `fzf` can be found within your PATH.
  
  docs.to-html
  `docs.to-html .path.to.ns doc-dir`                   Render
                                                       all docs
                                                       contained
                                                       within
                                                       the
                                                       namespace
                                                       `.path.to.ns`,
                                                       no matter
                                                       how deep,
                                                       to html
                                                       files in
                                                       `doc-dir`
                                                       in the
                                                       directory
                                                       UCM was
                                                       run from.
  `docs.to-html project0/branch0:a.path /tmp/doc-dir`  Renders
                                                       all docs
                                                       anywhere
                                                       in the
                                                       namespace
                                                       `a.path`
                                                       from
                                                       `branch0`
                                                       of
                                                       `project0`
                                                       to html
                                                       in
                                                       `/tmp/doc-dir`.
  
  edit
  `edit foo` prepends the definition of `foo` to the top of the most recently saved file.
  `edit` without arguments invokes a search to select a definition for editing, which requires that `fzf` can be found within your PATH.
  
  edit.namespace
  `edit.namespace` will load all terms and types contained within the current namespace into your scratch file. This includes definitions in namespaces, but excludes libraries.
  `edit.namespace ns1 ns2 ...` loads the terms and types contained within the provided namespaces.
  
  edit.new
  Like `edit`, but adds a new fold line below the definitions.
  
  find
  `find`                           lists all definitions in the
                                   current namespace.
  `find foo`                       lists all definitions with a
                                   name similar to 'foo' in the
                                   current namespace (excluding
                                   those under 'lib').
  `find foo bar`                   lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the current
                                   namespace (excluding those
                                   under 'lib').
  `find-in namespace`              lists all definitions in the
                                   specified subnamespace.
  `find-in namespace foo bar`      lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the specified
                                   subnamespace.
  find.all foo                     lists all definitions with a
                                   name similar to 'foo' in the
                                   current namespace (including
                                   one level of 'lib').
  `find-in.all namespace`          lists all definitions in the
                                   specified subnamespace
                                   (including one level of its
                                   'lib').
  `find-in.all namespace foo bar`  lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the specified
                                   subnamespace (including one
                                   level of its 'lib').
  debug.find.global foo            Iteratively searches all
                                   projects and branches and
                                   lists all definitions with a
                                   name similar to 'foo'. Note
                                   that this is a very slow
                                   operation.
  
  find-in
  `find`                           lists all definitions in the
                                   current namespace.
  `find foo`                       lists all definitions with a
                                   name similar to 'foo' in the
                                   current namespace (excluding
                                   those under 'lib').
  `find foo bar`                   lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the current
                                   namespace (excluding those
                                   under 'lib').
  `find-in namespace`              lists all definitions in the
                                   specified subnamespace.
  `find-in namespace foo bar`      lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the specified
                                   subnamespace.
  find.all foo                     lists all definitions with a
                                   name similar to 'foo' in the
                                   current namespace (including
                                   one level of 'lib').
  `find-in.all namespace`          lists all definitions in the
                                   specified subnamespace
                                   (including one level of its
                                   'lib').
  `find-in.all namespace foo bar`  lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the specified
                                   subnamespace (including one
                                   level of its 'lib').
  debug.find.global foo            Iteratively searches all
                                   projects and branches and
                                   lists all definitions with a
                                   name similar to 'foo'. Note
                                   that this is a very slow
                                   operation.
  
  find-in.all
  `find`                           lists all definitions in the
                                   current namespace.
  `find foo`                       lists all definitions with a
                                   name similar to 'foo' in the
                                   current namespace (excluding
                                   those under 'lib').
  `find foo bar`                   lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the current
                                   namespace (excluding those
                                   under 'lib').
  `find-in namespace`              lists all definitions in the
                                   specified subnamespace.
  `find-in namespace foo bar`      lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the specified
                                   subnamespace.
  find.all foo                     lists all definitions with a
                                   name similar to 'foo' in the
                                   current namespace (including
                                   one level of 'lib').
  `find-in.all namespace`          lists all definitions in the
                                   specified subnamespace
                                   (including one level of its
                                   'lib').
  `find-in.all namespace foo bar`  lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the specified
                                   subnamespace (including one
                                   level of its 'lib').
  debug.find.global foo            Iteratively searches all
                                   projects and branches and
                                   lists all definitions with a
                                   name similar to 'foo'. Note
                                   that this is a very slow
                                   operation.
  
  find.all
  `find`                           lists all definitions in the
                                   current namespace.
  `find foo`                       lists all definitions with a
                                   name similar to 'foo' in the
                                   current namespace (excluding
                                   those under 'lib').
  `find foo bar`                   lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the current
                                   namespace (excluding those
                                   under 'lib').
  `find-in namespace`              lists all definitions in the
                                   specified subnamespace.
  `find-in namespace foo bar`      lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the specified
                                   subnamespace.
  find.all foo                     lists all definitions with a
                                   name similar to 'foo' in the
                                   current namespace (including
                                   one level of 'lib').
  `find-in.all namespace`          lists all definitions in the
                                   specified subnamespace
                                   (including one level of its
                                   'lib').
  `find-in.all namespace foo bar`  lists all definitions with a
                                   name similar to 'foo' or
                                   'bar' in the specified
                                   subnamespace (including one
                                   level of its 'lib').
  debug.find.global foo            Iteratively searches all
                                   projects and branches and
                                   lists all definitions with a
                                   name similar to 'foo'. Note
                                   that this is a very slow
                                   operation.
  
  find.all.verbose
  `find.all.verbose` searches for definitions like `find.all`, but includes hashes and aliases in the results.
  
  find.verbose
  `find.verbose` searches for definitions like `find`, but includes hashes and aliases in the results.
  
  fork (or copy.namespace)
  `fork src dest`                                      creates
                                                       the
                                                       namespace
                                                       `dest` as
                                                       a copy of
                                                       `src`.
  `fork project0/branch0:a.path project1/branch1:foo`  creates
                                                       the
                                                       namespace
                                                       `foo` in
                                                       `branch1`
                                                       of
                                                       `project1`
                                                       as a copy
                                                       of
                                                       `a.path`
                                                       in
                                                       `project0/branch0`.
  `fork srcproject/srcbranch dest`                     creates
                                                       the
                                                       namespace
                                                       `dest` as
                                                       a copy of
                                                       the
                                                       branch
                                                       `srcbranch`
                                                       of
                                                       `srcproject`.
  
  help (or ?)
  `help` shows general help and `help <cmd>` shows help for one command.
  
  help-topics (or help-topic)
  `help-topics` lists all topics and `help-topics <topic>` shows an explanation of that topic.
  
  history
  `history`                     Shows the history of the current
                                path.
  `history .foo`                Shows history of the path .foo.
  `history #9dndk3kbsk13nbpeu`  Shows the history of the
                                namespace with the given hash.
                                The full hash must be provided.
  
  io.test (or test.io)
  `io.test mytest`  Runs `!mytest`, where `mytest` is a delayed
                    test that can use the `IO` and `Exception`
                    abilities.
  
  io.test.all (or test.io.all)
  `io.test.all`  runs unit tests for the current branch that use
                 IO
  
  lib.install (or install.lib)
  The `lib.install` command installs a dependency into the `lib`
  namespace.
  
  `lib.install @unison/base/releases/latest`  installs the
                                              latest release of
                                              `@unison/base`
  `lib.install @unison/base/releases/3.0.0`   installs version
                                              3.0.0 of
                                              `@unison/base`
  `lib.install @unison/base/topic`            installs the
                                              `topic` branch of
                                              `@unison/base`
  
  list (or ls, dir)
  `list`       lists definitions and namespaces at the current
               level of the current namespace.
  `list foo`   lists the 'foo' namespace.
  `list .foo`  lists the '.foo' namespace.
  
  load
  `load`                 parses, typechecks, and evaluates the
                         most recent scratch file.
  `load <scratch file>`  parses, typechecks, and evaluates the
                         given scratch file.
  
  merge
  `merge /branch` merges `branch` into the current branch
  
  merge.commit (or commit.merge)
  `merge.commit` merges a temporary branch created by the
  `merge` command back into its parent branch, and removes the
  temporary branch.
  
  For example, if you've done `merge topic` from main, then
  `merge.commit` is equivalent to doing
  
    * switch /main
    * merge /merge-topic-into-main
    * delete.branch /merge-topic-into-main
  
  move (or rename)
  `move foo bar` renames the term, type, and namespace foo to bar.
  
  move.namespace (or rename.namespace)
  `move.namespace foo bar` renames the path `foo` to `bar`.
  
  move.term (or rename.term)
  `move.term foo bar` renames `foo` to `bar`.
  
  move.type (or rename.type)
  `move.type foo bar` renames `foo` to `bar`.
  
  names
  `names foo` List all known names for `foo` in the current
  branch.
  
  namespace.dependencies
  List the external dependencies of the specified namespace.
  
  project.create (or create.project)
  `project.create`      creates a project with a random name
  `project.create foo`  creates a project named `foo`
  
  project.reflog (or reflog.project)
  `project.reflog` lists all the changes that have affected any branches in the current project.
  `project.reflog myproject` lists all the changes that have affected any branches in myproject.
  
  project.rename (or rename.project)
  `project.rename foo`  renames the current project to `foo`
  
  projects (or list.project, ls.project, project.list)
  List projects.
  
  pull
  The `pull` command merges a remote namespace into a local
  branch
  
  `pull @unison/base/main`                merges the branch
                                          `main` of the Unison
                                          Share hosted project
                                          `@unison/base` into
                                          the current branch
  `pull @unison/base/main my-base/topic`  merges the branch
                                          `main` of the Unison
                                          Share hosted project
                                          `@unison/base` into
                                          the branch `topic` of
                                          the local `my-base`
                                          project
  
  where `remote` is a project or project branch, such as:
    Project (defaults to the /main branch) `@unison/base`
    Project Branch                         `@unison/base/feature`
    Contributor Branch                     `@unison/base/@johnsmith/feature`
    Project Release                        `@unison/base/releases/1.0.0`
  
  pull.without-history
  The `pull.without-history` command merges a remote namespace
  into a local branch without including the remote's history.
  This usually results in smaller codebase sizes.
  
  `pull.without-history @unison/base/main`                merges
                                                          the
                                                          branch
                                                          `main`
                                                          of the
                                                          Unison
                                                          Share
                                                          hosted
                                                          project
                                                          `@unison/base`
                                                          into
                                                          the
                                                          current
                                                          branch
  `pull.without-history @unison/base/main my-base/topic`  merges
                                                          the
                                                          branch
                                                          `main`
                                                          of the
                                                          Unison
                                                          Share
                                                          hosted
                                                          project
                                                          `@unison/base`
                                                          into
                                                          the
                                                          branch
                                                          `topic`
                                                          of the
                                                          local
                                                          `my-base`
                                                          project
  
  where `remote` is a project or project branch, such as:
    Project (defaults to the /main branch) `@unison/base`
    Project Branch                         `@unison/base/feature`
    Contributor Branch                     `@unison/base/@johnsmith/feature`
    Project Release                        `@unison/base/releases/1.0.0`
  
  push
  The `push` command merges a local project or namespace into a
  remote project or namespace.
  
  `push <remote> <local>`  publishes the contents of a local
                           namespace or branch into a remote
                           namespace or branch.
  `push <remote>`          publishes the current namespace or
                           branch into a remote namespace or
                           branch
  `push`                   publishes the current namespace or
                           branch. Remote mappings for
                           namespaces are configured in your
                           `.unisonConfig` at the key
                           `RemoteMappings.<namespace>` where
                           `<namespace>` is the current
                           namespace. Remote mappings for
                           branches default to the branch that
                           you cloned from or pushed to
                           initially. Otherwise, it is pushed to
                           @<user handle>/<local project name>
  
  where `remote` is a project or project branch, such as:
    Project (defaults to the /main branch) `@unison/base`
    Project Branch                         `@unison/base/feature`
    Contributor Branch                     `@unison/base/@johnsmith/feature`
  
  push.create
  The `push.create` command pushes a local namespace to an empty
  remote namespace.
  
  `push.create remote local`  pushes the contents of the local
                              namespace `local` into the empty
                              remote namespace `remote`.
  `push.create remote`        publishes the current namespace
                              into the empty remote namespace
                              `remote`
  `push.create`               publishes the current namespace
                              into the remote namespace
                              configured in your `.unisonConfig`
                              at the key
                              `RemoteMappings.<namespace>` where
                              `<namespace>` is the current
                              namespace, then publishes the
                              current namespace to that
                              location.
  
  where `remote` is a project or project branch, such as:
    Project (defaults to the /main branch) `@unison/base`
    Project Branch                         `@unison/base/feature`
    Contributor Branch                     `@unison/base/@johnsmith/feature`
  
  quit (or exit, :q)
  Exits the Unison command line interface.
  
  reflog (or reflog.branch, branch.reflog)
  `reflog` lists all the changes that have affected the current branch.
  `reflog /mybranch` lists all the changes that have affected /mybranch.
  
  reflog.global
  `reflog.global` lists all recent changes across all projects and branches.
  
  release.draft (or draft.release)
  Draft a release.
  
  reset
  `reset #pvfd222s8n`         reset the current namespace to the
                              hash `#pvfd222s8n`
  `reset foo`                 reset the current namespace to the
                              state of the `foo` namespace.
  `reset #pvfd222s8n /topic`  reset the branch `topic` of the
                              current project to the causal
                              `#pvfd222s8n`.
  
  If you make a mistake using reset, consult the `reflog`
  command and use another `reset` command to return to a
  previous state.
  
  rewrite (or sfind.replace)
  `rewrite rule1` rewrites definitions in the latest scratch file.
  
  The argument `rule1` must refer to a `@rewrite` block or a
  function that immediately returns a `@rewrite` block. It can
  be in the codebase or scratch file. An example:
  
      rule1 x = @rewrite term x + 1 ==> Nat.increment x
  
  Here, `x` will stand in for any expression wherever this
  rewrite is applied, so this rule will match `(42+10+11) + 1`
  and replace it with `Nat.increment (42+10+11)`.
  
  See https://unison-lang.org/learn/structured-find to learn more.
  
  Also see the related command `rewrite.find`
  
  rewrite.find (or sfind)
  `rewrite.find rule1` finds definitions that match any of the
  left side(s) of `rule` in the current namespace.
  
  The argument `rule1` must refer to a `@rewrite` block or a
  function that immediately returns a `@rewrite` block. It can
  be in the codebase or scratch file. An example:
  
      -- right of ==> is ignored by this command
      rule1 x = @rewrite term x + 1 ==> ()
  
  Here, `x` will stand in for any expression, so this rule will
  match `(42+10+11) + 1`.
  
  See https://unison-lang.org/learn/structured-find to learn more.
  
  Also see the related command `rewrite`
  
  run
  `run mymain args...`  Runs `!mymain`, where `mymain` is
                        searched for in the most recent
                        typechecked file, or in the codebase.
                        Any provided arguments will be passed as
                        program arguments as though they were
                        provided at the command line when
                        running mymain as an executable.
  
  run.native
  `run.native main args`  Executes !main using native
                          compilation via scheme.
  
  switch
  `switch`          opens an interactive selector to pick a
                    project and branch
  `switch foo/bar`  switches to the branch `bar` in the project
                    `foo`
  `switch foo/`     switches to the last branch you visited in
                    the project `foo`
  `switch /bar`     switches to the branch `bar` in the current
                    project
  
  test
  `test`      runs unit tests for the current branch
  `test foo`  runs unit tests for the current branch defined in
              namespace `foo`
  
  test.all
  `test.all` runs unit tests for the current branch (including the `lib` namespace).
  
  text.find (or grep)
  `text.find token1 "99" token2` finds terms with literals (text
  or numeric) containing `token1`, `99`, and `token2`.
  
  Numeric literals must be quoted (ex: "42") but single words
  need not be quoted.
  
  Use `text.find.all` to include search of `lib`.
  
  text.find.all (or grep.all)
  `text.find.all token1 "99" token2` finds terms with literals
  (text or numeric) containing `token1`, `99`, and `token2`.
  
  Numeric literals must be quoted (ex: "42") but single words
  need not be quoted.
  
  Use `text.find` to exclude `lib` from search.
  
  todo
  `todo` lists the current namespace's outstanding issues,
  including conflicted names, dependencies with missing names,
  and merge precondition violations.
  
  ui
  `ui` opens the Local UI in the default browser.
  
  undo
  `undo` reverts the most recent change to the codebase.
  
  update
  Adds everything in the most recently typechecked file to the
  namespace, replacing existing definitions having the same
  name, and attempts to update all the existing dependents
  accordingly. If the process can't be completed automatically,
  the dependents will be added back to the scratch file for your
  review.
  
  update.old
  `update.old` works like `add`, except that if a definition in
  the file has the same name as an existing definition, the name
  gets updated to point to the new definition. If the old
  definition has any dependents, `update` will add those
  dependents to a refactoring session, specified by an optional
  patch.`update.old`                  adds all definitions in
                                the .u file, noting replacements
                                in the default patch for the
                                current namespace.
  `update.old <patch>`          adds all definitions in the .u
                                file, noting replacements in the
                                specified patch.
  `update.old <patch> foo bar`  adds `foo`, `bar`, and their
                                dependents from the .u file,
                                noting any replacements into the
                                specified patch.
  
  update.old.nopatch
  `update.old.nopatch` works like `update.old`, except it
  doesn't add a patch entry for any updates. Use this when you
  want to make changes to definitions without pushing those
  changes to dependents beyond your codebase. An example is when
  updating docs, or when updating a term you just added.`update.old.nopatch`          updates
                                all definitions in the .u file.
  `update.old.nopatch foo bar`  updates `foo`, `bar`, and their
                                dependents from the .u file.
  
  update.old.preview
  `update.old.preview` previews updates to the codebase from the most recently typechecked file. This command only displays cached typechecking results. Use `load` to reparse & typecheck the file if the context has changed.
  
  upgrade
  `upgrade old new` upgrades library dependency `lib.old` to
  `lib.new`, and, if successful, deletes `lib.old`.
  
  upgrade.commit (or commit.upgrade)
  `upgrade.commit` merges a temporary branch created by the
  `upgrade` command back into its parent branch, and removes the
  temporary branch.
  
  For example, if you've done `upgrade foo bar` from main, then
  `upgrade.commit` is equivalent to doing
  
    * switch /main
    * merge /upgrade-foo-to-bar
    * delete.branch /upgrade-foo-to-bar
  
  version
  Print the version of unison you're running
  
  view
  `view foo` shows definitions named `foo` within your current
  namespace.
  `view` without arguments invokes a search to select
  definitions to view, which requires that `fzf` can be found
  within your PATH.
   
  Supports glob syntax, where ? acts a wildcard, so
  `view List.?` will show `List.map`, `List.filter`, etc, but
  not `List.map.doc` (since ? only matches 1 name segment).
  
  view.global
  `view.global foo` prints definitions of `foo` within your codebase.
  `view.global` without arguments invokes a search to select definitions to view, which requires that `fzf` can be found within your PATH.

scratch/main> help-topics

  🌻
  
  Here's a list of topics I can tell you more about: 
  
    filestatus
    messages.disallowedAbsolute
    namespaces
    projects
    remotes
    testcache
  
  Example: use `help-topics filestatus` to learn more about that topic.

scratch/main> help-topic filestatus

  📓
  
  Here's a list of possible status messages you might see for
  definitions in a .u file.
  
  needs update         A definition with the same name as an
                       existing definition. Doing `update`
                       instead of `add` will turn this failure
                       into a successful update.
                       
  term/ctor collision  A definition with the same name as an
                       existing constructor for some data type.
                       Rename your definition or the data type
                       before trying again to `add` or `update`.
                       
  ctor/term collision  A type defined in the file has a
                       constructor that's named the same as an
                       existing term. Rename that term or your
                       constructor before trying again to `add`
                       or `update`.
                       
  blocked              This definition was blocked because it
                       dependended on a definition with a failed
                       status.
                       
  extra dependency     This definition was added because it was
                       a dependency of a definition explicitly
                       selected.

scratch/main> help-topic messages.disallowedAbsolute

  🤖
  
  Although I can understand absolute (ex: .foo.bar) or relative
  (ex: util.math.sqrt) references to existing definitions
  (help namespaces to learn more), I can't yet handle giving new
  definitions with absolute names in a .u file.
  
  As a workaround, you can give definitions with a relative name
  temporarily (like `exports.blah.foo`) and then use `move.*`.

scratch/main> help-topic namespaces

  🧐
  
  There are two kinds of namespaces, absolute, such as (.foo.bar
  or .base.math.+) and relative, such as (math.sqrt or
  util.List.++).
  
  Relative names are converted to absolute names by prepending
  the current namespace. For example, if your Unison prompt
  reads:
  
    .foo.bar>
  
  and your .u file looks like:
  
    x = 41
  
  then doing an add will create the definition with the absolute
  name .foo.bar.x = 41
  
  and you can refer to x by its absolute name .foo.bar.x
  elsewhere in your code. For instance:
  
    answerToLifeTheUniverseAndEverything = .foo.bar.x + 1

scratch/main> help-topic projects

  A project is a versioned collection of code that can be
  edited, published, and depended on other projects. Unison
  projects are analogous to Git repositories.
  
  project.create create a new project
  projects       list all your projects
  branch         create a new workstream
  branches       list all your branches
  merge          merge one branch into another
  switch         switch to a project or branch
  push           upload your changes to Unison Share
  pull           download code(/changes/updates) from Unison Share
  clone          download a Unison Share project or branch for contribution
  
  Tip: Use `help project.create` to learn more.
  
  For full documentation, see
  https://unison-lang.org/learn/projects

scratch/main> help-topic remotes

  🤖
  
  Local projects may be associated with at most one remote
  project on Unison Share. When this relationship is
  established, it becomes the default argument for a number of
  share commands. For example, running `push` or `pull` in a
  project with no arguments will push to or pull from the
  associated remote, if it exists.
  
  This association is created automatically on when a project is
  created by `clone`. If the project was created locally then
  the relationship will be established on the first `push`.

scratch/main> help-topic testcache

  🎈
  
  Unison caches the results of test> watch expressions. Since
  these expressions are pure and always yield the same result
  when evaluated, there's no need to run them more than once!
  
  A test is rerun only if it has changed, or if one of the
  definitions it depends on has changed.

```
We should add a command to show help for hidden commands also.

