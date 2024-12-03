``` ucm :hide
scratch/main> builtins.merge
```

``` unison

up = 0xs0123456789abcdef
down = 0xsfedcba9876543210

-- | Generated with:
--     openssl genrsa -out private_key.pem 1024
--     openssl rsa -in private_key.pem -outform DER | xxd -p
secret = 0xs30820276020100300d06092a864886f70d0101010500048202603082025c02010002818100a7104b2f20725896076e629ccedbcd6907b16694c6e3d8768b5e0e685670b49616e796c588e5aafb92ef986c1a42c021fed0bdc99212c969cdab98087a0ee4c2f4acd4b6049a87a96afc45668329a3cf21a86fb13b488bbe9fefa1cd5a459014f0d0101378e9661e11b73acf54c8a91141ac90309e7fb6ed69b4e63230ab291502030100010281807cdc23a4fc3619d93f8293b728af848d0c0fdd603269d5bd7b99f760a9c22065d08693dbdcddf1f5863306133d694819e04d789aef4e95343b601507b8d9eac4492e6d7031b035c5d84eceaa9686b292712632d33b3303af84314d7920bc3d45f90d7818fc2587b129196d378ee4ed3e6b8d9010d504bb6470ff53e7c5fb17a1024100d67cbcf113d24325fcef12a778dc47c7060055290b68287649ef092558daccb61c4e7bc290740b75a29d4356dcbd66d18b0860dbff394cc8ff3d94d57617adbd024100c765d8261dd3d8e0d3caf11ab7b212eed181354215687ca6387283e4f0be16e79c8f298be0a70c7734dea78ea65128517d693cabfa4c0ff5328f2abb85d2023902403ca41dc347285e65c22251b2d9bfe5e7463217e1b7e0e5f7b3a58a7f6da4c6d60220ca6ad2ee8c42e10bf77afa83ee2af6551315800e52404db1ba7fb398b43d02410084877d85c0177933ddb12a554eb8edfa8b872c85d2c2d2ee8be019280696e19469ab81bab5c371f69d4e4be1f54b45d7fbda017870f1333e0eafb78051ee8689024061f694c12e934c44b7734f62d1b2a3d3624a4980e1b8e066d78dbabd2436654fbb9d9701425900daaafa1e031310e8a580520bb9e1c1288c669fce252bad1e65

-- | Generated with:
--     openssl rsa -in private_key.pem -outform DER -pubout | xxd -p
publicKey = 0xs30819f300d06092a864886f70d010101050003818d0030818902818100a7104b2f20725896076e629ccedbcd6907b16694c6e3d8768b5e0e685670b49616e796c588e5aafb92ef986c1a42c021fed0bdc99212c969cdab98087a0ee4c2f4acd4b6049a87a96afc45668329a3cf21a86fb13b488bbe9fefa1cd5a459014f0d0101378e9661e11b73acf54c8a91141ac90309e7fb6ed69b4e63230ab29150203010001

incorrectPublicKey = 0xs30819f300d06092a864886f70d010101050003818d0030818902818100a7104b2f20725896076e629ccedbcd6907b16694c6e3d8768b5e0e685670b49616e796c588e5aafb92ef986c1a42c021fed0bdc99212c969cdab98087a0ee4c2f4acd4b6049a87a96afc45668329a3cf21a86fb13b488bbe9fefa1cd5a459014f0d0101378e9661e11b73acf54c8a91141ac90309e7fb6ed69b4e63230ab29150203010002

message = up ++ down ++ up ++ down ++ down ++ up ++ down ++ up

signature = crypto.Rsa.sign.impl secret message

sigOkay = match signature with
  Left err -> Left err
  Right sg -> crypto.Rsa.verify.impl publicKey message sg

sigKo = match signature with
  Left err -> Left err
  Right sg -> crypto.Rsa.verify.impl incorrectPublicKey message sg

> signature
> sigOkay
> sigKo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:

    ⍟ These new definitions are ok to `add`:
    
      down               : Bytes
      incorrectPublicKey : Bytes
      message            : Bytes
      publicKey          : Bytes
      secret             : Bytes
      sigKo              : Either Failure Boolean
      sigOkay            : Either Failure Boolean
      signature          : Either Failure Bytes
      up                 : Bytes

  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    28 | > signature
           ⧩
           Right
             0xs84b02b6bb0e1196b65378cb12b727f7b4b38e5979f0632e8a51cfab088827f6d3da4221788029f75a0a5f4d740372cfa590462888a1189bbd9de9b084f26116640e611af5a1a17229beec7fb2570887181bbdced8f0ebfec6cad6bdd318a616ba4f01c90e1436efe44b18417d18ce712a0763be834f8c76e0c39b2119b061373

    29 | > sigOkay
           ⧩
           Right true

    30 | > sigKo
           ⧩
           Right false
```
