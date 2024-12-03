``` ucm :hide
scratch/main> builtins.mergeio
```

Demonstrating `create.author`:

``` ucm
scratch/main> create.author alicecoder "Alice McGee"

  Added definitions:

    1. metadata.authors.alicecoder          : Author
    2. metadata.copyrightHolders.alicecoder : CopyrightHolder
    3. metadata.authors.alicecoder.guid     : GUID

  Tip: Add License values for alicecoder under metadata.

scratch/main> find alicecoder

  1. metadata.authors.alicecoder : Author
  2. metadata.copyrightHolders.alicecoder : CopyrightHolder
  3. metadata.authors.alicecoder.guid : GUID
```
