The Unison codebase format needs to be able to store metadata about definitions it contains, such as:

  - Author, copyright holder
  - Creation date
  - License
  - API docs
  - Boolean indicating whether a definition is a test, needed to support incremental test evaluation
  - Comments that annotate subpaths of the definition
  - ...

Some desired features:

  - We probably won't know all the kinds of metadata in advance, so having it be extensible would be good.
  - Metadata should probably be versioned. (Example: what if you want to change the license of a definition?)

A simple proposal is to just add metadata information at each level of the versioned namespace tree:

``` Haskell
-- Metadata is always just a link to some other term
newtype Metadata = Metadata Reference
newtype MetadataType = MetadataType Text -- "License", "Creation date", etc

data Branch0 = 
  Branch0 { _terms    :: Relation NameSegment Referent
          , _types    :: Relation NameSegment Reference
          , _edits    :: ...
          , _metadata :: Relation (MetadataType, Referent) Metadata }
```

That's it. Metadata is just a "link", a lightweight reference to some other definition.

We don't try to make `MetadataType` more strongly typed. It's just a string, its meaning determined by convention. For instance, the default CLI viewer can look for an "API docs" key, and use that in its display.

Nothing special for the on disk format, it can just be encoded the same way as the other relations in the Branch0.
