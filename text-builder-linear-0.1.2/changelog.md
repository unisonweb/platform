## 0.1.2

* Fix unsound behaviour caused by inlining of `runBuffer` / `runBufferBS`
  and CSE (common subexpression elimination).
* Fix hexadecimal builder, looping on negative inputs.
* Fix decimal builder for non-standard bitness of the input.
* Add `(#<|)` and deprecate `(|>#)`.
* Add `newEmptyBuffer`.
* Add `prependChars` and `appendChars`.
* Add `justifyLeft`, `justifyRight` and `center`.

## 0.1.1.1

* Support `text-2.1`.

## 0.1.1

* Introduce `ByteString` backend (thanks @oberblastmeister for the idea).
* Fix decimal builder for 30- and 31-bit wide types.
* Speed up decimal builder on aarch64.
* Speed up hexadecimal builder.
* Support 32-bit architectures.

## 0.1

* Initial release.
