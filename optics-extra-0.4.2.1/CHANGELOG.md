# optics-extra-0.4.2.1 (2022-05-20)
* Fix for previous release when used with `mtl-2.3` and `transformers-0.5`.

# optics-extra-0.4.2 (2022-05-19)
* Allow `transformers-0.6` and `mtl-2.3`

  Note that `optics-extra` no longer defines `Zoom` instances for `ErrorT` or `ListT` when
  building with `mtl-2.3` or later. This is because `MonadState` is a superclass of
  `Zoom`, and the `MonadState` instances for `ErrorT` and `ListT` were removed in
  `mtl-2.3`. Be watchful of this if you build `optics-extra` with `mtl-2.3` (or
  later) combined with an older version of `transformers` (pre-0.6) that defines
  `ErrorT` or `ListT`.  Similarly for `Magnify` and `MagnifyMany`.

# optics-extra-0.4.1 (2022-03-22)
* Add support for GHC-9.2

# optics-extra-0.4 (2021-02-22)
* Add support for GHC-9.0

# optics-extra-0.3 (2020-04-15)
* `optics-core-0.3` compatible release
* GHC-8.10 support
* Use stricter `uncurry'` for better performance

# optics-extra-0.2 (2019-10-18)
* `optics-core-0.2` compatible release
* Move `use` from `Optics.View` to `Optics.State` and restrict its type
* Add `preuse` to `Optics.State`
* Rename `use`, `uses`, `listening` and `listenings` to reflect the fact that
  they have `ViewResult`-generalised types
* Depend on new `indexed-profunctors` package

# optics-extra-0.1 (2019-09-02)
* Initial release
