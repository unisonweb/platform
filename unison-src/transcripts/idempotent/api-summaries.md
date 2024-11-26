# Definition Summary APIs

``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison :hide
nat : Nat
nat = 42
doc : Doc2
doc = {{ Hello }}
test> mytest = [Test.Result.Ok "ok"]
func : Text -> Text
func x = x ++ "hello"

funcWithLongType : Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text
funcWithLongType a b c d e f g h = a ++ b ++ c ++ d ++ e ++ f ++ g ++ h

structural type Thing = This Nat | That
structural type Maybe a = Nothing | Just a

structural ability Stream s where
  send : s -> ()
```

``` ucm :hide
scratch/main> add

scratch/main> alias.type ##Nat Nat

scratch/main> alias.term ##IO.putBytes.impl.v3 putBytesImpl
```

## Term Summary APIs

``` api
-- term
GET /api/projects/scratch/branches/main/definitions/terms/by-hash/@qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8/summary?name=nat
  {
      "displayName": "nat",
      "hash": "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "contents": "##Nat",
                      "tag": "TypeReference"
                  },
                  "segment": "Nat"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "Plain"
  }
-- term without name uses hash
GET /api/projects/scratch/branches/main/definitions/terms/by-hash/@qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8/summary
  {
      "displayName": "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8",
      "hash": "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "contents": "##Nat",
                      "tag": "TypeReference"
                  },
                  "segment": "Nat"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "Plain"
  }
-- doc
GET /api/projects/scratch/branches/main/definitions/terms/by-hash/@icfnhas71n8q5rm7rmpe51hh7bltsr7rb4lv7qadc4cbsifu1mhonlqj2d7836iar2ptc648q9p4u7hf40ijvld574421b6u8gpu0lo/summary?name=doc
  {
      "displayName": "doc",
      "hash": "#icfnhas71n8q5rm7rmpe51hh7bltsr7rb4lv7qadc4cbsifu1mhonlqj2d7836iar2ptc648q9p4u7hf40ijvld574421b6u8gpu0lo",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "contents": "#ej86si0ur1lsjade71dojr25phk9bbom9rdks6dltolos5tjivakujcriqe02npba53n9gd7tkh8bmv08ttjb9t35lq2ch5heshqcs0",
                      "tag": "TypeReference"
                  },
                  "segment": "Doc2"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "Doc"
  }
-- test
GET /api/projects/scratch/branches/main/definitions/terms/by-hash/@u17p9803hdibisou6rlr1sjbccdossgh7vtkd03ovlvnsl2n91lq94sqhughc62tnrual2jlrfk922sebp4nm22o7m5u9j40emft8r8/summary?name=mytest
  {
      "displayName": "mytest",
      "hash": "#u17p9803hdibisou6rlr1sjbccdossgh7vtkd03ovlvnsl2n91lq94sqhughc62tnrual2jlrfk922sebp4nm22o7m5u9j40emft8r8",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "tag": "DelimiterChar"
                  },
                  "segment": "["
              },
              {
                  "annotation": {
                      "contents": "#aql7qk3iud6vs4cvu43aimopoosgk0fnipibdkc3so13adencmibgfn0u5c01r0adei55nkl3ttsjhl8gbj7tr4gnpj63g64ftbq6s0",
                      "tag": "TypeReference"
                  },
                  "segment": "Result"
              },
              {
                  "annotation": {
                      "tag": "DelimiterChar"
                  },
                  "segment": "]"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "Test"
  }
-- function
GET /api/projects/scratch/branches/main/definitions/terms/by-hash/@6ee6j48hk3eovokflkgbmpbfr3oqj4hedqn8ocg3i4i0ko8j7nls7njjirmnh4k2bg8h95seaot798uuloqk62u2ttiqoceulkbmq2o/summary?name=func
  {
      "displayName": "func",
      "hash": "#6ee6j48hk3eovokflkgbmpbfr3oqj4hedqn8ocg3i4i0ko8j7nls7njjirmnh4k2bg8h95seaot798uuloqk62u2ttiqoceulkbmq2o",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "Plain"
  }
-- constructor
GET /api/projects/scratch/branches/main/definitions/terms/by-hash/@altimqs66j3dh94dpab5pg7j5adjrndq61n803j7fg0v0ohdiut6or66bu1fiongpd45s5euiuo8ru47b928aqv8osln1ikdeg05hq0@d0/summary?name=Thing.This
  {
      "displayName": "Thing.This",
      "hash": "#altimqs66j3dh94dpab5pg7j5adjrndq61n803j7fg0v0ohdiut6or66bu1fiongpd45s5euiuo8ru47b928aqv8osln1ikdeg05hq0#0",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "contents": "##Nat",
                      "tag": "TypeReference"
                  },
                  "segment": "Nat"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "#altimqs66j3dh94dpab5pg7j5adjrndq61n803j7fg0v0ohdiut6or66bu1fiongpd45s5euiuo8ru47b928aqv8osln1ikdeg05hq0",
                      "tag": "TypeReference"
                  },
                  "segment": "Thing"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "DataConstructor"
  }
-- Long type signature
GET /api/projects/scratch/branches/main/definitions/terms/by-hash/@ieskgcjjvuegpecq9pbha59ttonke7pf31keeq0jlh31ijkfq00e06fdi36ae90u24pjva6ucqdbedropjgi3g3b75nu76ll5ls8ke8/summary?name=funcWithLongType
  {
      "displayName": "funcWithLongType",
      "hash": "#ieskgcjjvuegpecq9pbha59ttonke7pf31keeq0jlh31ijkfq00e06fdi36ae90u24pjva6ucqdbedropjgi3g3b75nu76ll5ls8ke8",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "Plain"
  }
-- Long type signature with render width
GET /api/projects/scratch/branches/main/definitions/terms/by-hash/@ieskgcjjvuegpecq9pbha59ttonke7pf31keeq0jlh31ijkfq00e06fdi36ae90u24pjva6ucqdbedropjgi3g3b75nu76ll5ls8ke8/summary?renderWidth=20&name=funcWithLongType
  {
      "displayName": "funcWithLongType",
      "hash": "#ieskgcjjvuegpecq9pbha59ttonke7pf31keeq0jlh31ijkfq00e06fdi36ae90u24pjva6ucqdbedropjgi3g3b75nu76ll5ls8ke8",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": "\n"
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": "\n"
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": "\n"
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": "\n"
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": "\n"
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": "\n"
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": "\n"
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              },
              {
                  "annotation": null,
                  "segment": "\n"
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Text",
                      "tag": "TypeReference"
                  },
                  "segment": "Text"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "Plain"
  }
-- Builtin Term
GET /api/projects/scratch/branches/main/definitions/terms/by-hash/@@IO.putBytes.impl.v3/summary?name=putBytesImpl
  {
      "displayName": "putBytesImpl",
      "hash": "##IO.putBytes.impl.v3",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "contents": "##Handle",
                      "tag": "TypeReference"
                  },
                  "segment": "Handle"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "##Bytes",
                      "tag": "TypeReference"
                  },
                  "segment": "Bytes"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "TypeOperator"
                  },
                  "segment": "->"
              },
              {
                  "annotation": {
                      "tag": "AbilityBraces"
                  },
                  "segment": "{"
              },
              {
                  "annotation": {
                      "contents": "##IO",
                      "tag": "TypeReference"
                  },
                  "segment": "IO"
              },
              {
                  "annotation": {
                      "tag": "AbilityBraces"
                  },
                  "segment": "}"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "#0o7mf021foma9acqdaibmlh1jidlijq08uf7f5se9tssttqs546pfunjpk6s31mqoq8s2o1natede8hkk6he45l95fibglidikt44v8",
                      "tag": "TypeReference"
                  },
                  "segment": "Either"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "#r29dja8j9dmjjp45trccchaata8eo1h6d6haar1eai74pq1jt4m7u3ldhlq79f7phfo57eq4bau39vqotl2h63k7ff1m5sj5o9ajuf8",
                      "tag": "TypeReference"
                  },
                  "segment": "Failure"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": null,
                  "segment": "("
              },
              {
                  "annotation": null,
                  "segment": ")"
              }
          ],
          "tag": "BuiltinObject"
      },
      "tag": "Plain"
  }
```

## Type Summary APIs

``` api
-- data
GET /api/projects/scratch/branches/main/definitions/types/by-hash/@altimqs66j3dh94dpab5pg7j5adjrndq61n803j7fg0v0ohdiut6or66bu1fiongpd45s5euiuo8ru47b928aqv8osln1ikdeg05hq0/summary?name=Thing
  {
      "displayName": "Thing",
      "hash": "#altimqs66j3dh94dpab5pg7j5adjrndq61n803j7fg0v0ohdiut6or66bu1fiongpd45s5euiuo8ru47b928aqv8osln1ikdeg05hq0",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "tag": "DataTypeModifier"
                  },
                  "segment": "structural"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "DataTypeKeyword"
                  },
                  "segment": "type"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "Thing",
                      "tag": "HashQualifier"
                  },
                  "segment": "Thing"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "Data"
  }
-- data with type args
GET /api/projects/scratch/branches/main/definitions/types/by-hash/@nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg/summary?name=Maybe
  {
      "displayName": "Maybe",
      "hash": "#nirp5os0q69o4e1u9p3t6mmq6l6otluefi3ksm7dhm0diidjvkkgl8o9bvnflbj0sanuvdusf34f1qrins3ktcaglpcqv9oums2slsg",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "tag": "DataTypeModifier"
                  },
                  "segment": "structural"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "DataTypeKeyword"
                  },
                  "segment": "type"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "Maybe",
                      "tag": "HashQualifier"
                  },
                  "segment": "Maybe"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "DataTypeParams"
                  },
                  "segment": "a"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "Data"
  }
-- ability
GET /api/projects/scratch/branches/main/definitions/types/by-hash/@rfi1v9429f9qluv533l2iba77aadttilrpmnhljfapfnfa6sru2nr8ibpqvib9nc4s4nb9s1as45upsfqfqe6ivqi2p82b2vd866it8/summary?name=Stream
  {
      "displayName": "Stream",
      "hash": "#rfi1v9429f9qluv533l2iba77aadttilrpmnhljfapfnfa6sru2nr8ibpqvib9nc4s4nb9s1as45upsfqfqe6ivqi2p82b2vd866it8",
      "summary": {
          "contents": [
              {
                  "annotation": {
                      "tag": "DataTypeModifier"
                  },
                  "segment": "structural"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "DataTypeKeyword"
                  },
                  "segment": "ability"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "contents": "Stream",
                      "tag": "HashQualifier"
                  },
                  "segment": "Stream"
              },
              {
                  "annotation": null,
                  "segment": " "
              },
              {
                  "annotation": {
                      "tag": "DataTypeParams"
                  },
                  "segment": "s"
              }
          ],
          "tag": "UserObject"
      },
      "tag": "Ability"
  }
-- builtin type
GET /api/projects/scratch/branches/main/definitions/types/by-hash/@@Nat/summary?name=Nat
  {
      "displayName": "Nat",
      "hash": "##Nat",
      "summary": {
          "contents": [
              {
                  "annotation": null,
                  "segment": "Nat"
              }
          ],
          "tag": "BuiltinObject"
      },
      "tag": "Data"
  }
```
