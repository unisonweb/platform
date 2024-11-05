module Unison.Codebase.Editor.HandleInput.RuntimeUtils
  ( evalUnisonTerm,
    evalUnisonTermE,
    evalPureUnison,
    displayDecompileErrors,
    selectRuntime,
    EvalMode (..)
  )
where
import Control.Lens
import Control.Monad.Reader (ask)
import Unison.ABT qualified as ABT
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output
import Unison.Codebase.Execute qualified as Codebase
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Parser.Ann (Ann (..))
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Util.Pretty qualified as P
import Unison.WatchKind qualified as WK

data EvalMode = Sandboxed | Permissive | Native

selectRuntime :: EvalMode -> Cli (Runtime.Runtime Symbol)
selectRuntime mode = ask <&> \case
  Cli.Env { runtime, sandboxedRuntime, nativeRuntime }
    | Permissive <- mode -> runtime
    | Sandboxed <- mode -> sandboxedRuntime
    | Native <- mode -> nativeRuntime

displayDecompileErrors :: [Runtime.Error] -> Cli ()
displayDecompileErrors errs = Cli.respond (PrintMessage msg)
  where
    msg =
      P.lines $
        [ P.warnCallout "I had trouble decompiling some results.",
          "",
          "The following errors were encountered:"
        ]
          ++ fmap (P.indentN 2) errs

-- | Evaluate a single closed definition.
evalUnisonTermE ::
  EvalMode ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli (Either Runtime.Error (Term Symbol Ann))
evalUnisonTermE mode ppe useCache tm = do
  Cli.Env {codebase} <- ask
  theRuntime <- selectRuntime mode

  let watchCache :: Reference.Id -> IO (Maybe (Term Symbol ()))
      watchCache ref = do
        maybeTerm <- Codebase.runTransaction codebase (Codebase.lookupWatchCache codebase ref)
        pure (Term.amap (\(_ :: Ann) -> ()) <$> maybeTerm)

  let cache = if useCache then watchCache else Runtime.noCache
  r <- liftIO (Runtime.evaluateTerm' (Codebase.codebaseToCodeLookup codebase) cache ppe theRuntime tm)
  when useCache do
    case r of
      Right (errs, tmr)
        -- don't cache when there were errors
        | null errs ->
            Cli.runTransaction do
              Codebase.putWatch
                WK.RegularWatch
                (Hashing.hashClosedTerm tm)
                (Term.amap (const Ann.External) tmr)
        | otherwise -> displayDecompileErrors errs
      Left _ -> pure ()
  pure $ r <&> Term.amap (\() -> Ann.External) . snd

-- | Evaluate a single closed definition.
evalUnisonTerm ::
  EvalMode ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli (Term Symbol Ann)
evalUnisonTerm mode ppe useCache tm =
  evalUnisonTermE mode ppe useCache tm & onLeftM \err ->
    Cli.returnEarly (EvaluationFailure err)

evalPureUnison ::
  Bool ->
  PPE.PrettyPrintEnv ->
  Bool ->
  Term Symbol Ann ->
  Cli (Either Runtime.Error (Term Symbol Ann))
evalPureUnison native ppe useCache tm =
  evalUnisonTermE mode ppe useCache tm'
  where
    mode = if native then Native else Permissive
    tm' =
      Term.iff
        a
        (Term.apps' (Term.builtin a "validateSandboxed") [allow, Term.delay a tm])
        tm
        (Term.app a (Term.builtin a "bug") (Term.text a msg))
    a = ABT.annotation tm
    allow = Term.list a [Term.termLink a (Referent.Ref (Reference.Builtin "Debug.toText"))]
    msg = "pure code can't perform I/O"
