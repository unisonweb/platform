-- | @debug.synhash.term@ input handler.
module Unison.Codebase.Editor.HandleInput.DebugSynhashTerm
  ( handleDebugSynhashTerm,
  )
where

import Control.Monad.Reader (ask)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import U.Util.Base32Hex qualified as Base32Hex
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty (prettyBase32Hex, prettyHash)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Hash (Hash)
import Unison.Hashable qualified as Hashable
import Unison.Merge.Synhash (hashBuiltinTermTokens, hashDerivedTermTokens)
import Unison.Name (Name)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty

handleDebugSynhashTerm :: Name -> Cli ()
handleDebugSynhashTerm name = do
  namespace <- Cli.getCurrentBranch0
  let names = Branch.toNames namespace
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  for_ (Names.refTermsNamed names name) \ref -> do
    maybeTokens <-
      case ref of
        Reference.Builtin builtin -> pure (Just (hashBuiltinTermTokens builtin))
        Reference.DerivedId refId -> do
          env <- ask
          Cli.runTransaction (Codebase.getTerm env.codebase refId) <&> \case
            Nothing -> Nothing
            Just term -> Just (hashDerivedTermTokens pped.unsuffixifiedPPE term)
    whenJust maybeTokens \tokens -> do
      let filename = Name.toText name <> "-" <> Reference.toText ref <> "-synhash-tokens.txt"
      let renderedTokens =
            tokens
              & map prettyToken
              & Pretty.lines
              & Pretty.toAnsiUnbroken
              & Text.pack
      liftIO (Text.writeFile (Text.unpack filename) renderedTokens)
      Cli.respond (Output'DebugSynhashTerm ref (Hashable.accumulate tokens) filename)

prettyToken :: Hashable.Token Hash -> Pretty ColorText
prettyToken = \case
  Hashable.Bytes bytes -> "0x" <> prettyBase32Hex (Base32Hex.fromByteString bytes)
  Hashable.Double n -> Pretty.string (show n)
  Hashable.Hashed h -> prettyHash h
  Hashable.Int n -> (if n >= 0 then "+" else mempty) <> Pretty.string (show n)
  Hashable.Nat n -> Pretty.string (show n)
  Hashable.Tag n -> "@" <> Pretty.string (show n)
  Hashable.Text s -> Pretty.string (show s)
