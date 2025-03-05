module Unison.LSP.Commands where

import Control.Lens hiding (List)
import Control.Monad.Except
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Language.LSP.Server (sendRequest)
import Unison.Debug qualified as Debug
import Unison.LSP.Types
import Unison.Prelude

supportedCommands :: [Text]
supportedCommands = ["replaceText"]

replaceText ::
  --  | The text displayed to the user for this command if used in a CodeLens
  Text ->
  TextReplacement ->
  Command
replaceText title tr = Command title "replaceText" (Just [Aeson.toJSON tr])

data TextReplacement = TextReplacement
  { range :: Range,
    -- Used in things like the editor's undo buffer
    description :: Text,
    replacementText :: Text,
    fileUri :: Uri
  }

instance Aeson.ToJSON TextReplacement where
  toJSON (TextReplacement range description replacementText fileUri) =
    Aeson.object
      [ "range" Aeson..= range,
        "description" Aeson..= description,
        "replacementText" Aeson..= replacementText,
        "fileUri" Aeson..= fileUri
      ]

instance Aeson.FromJSON TextReplacement where
  parseJSON = Aeson.withObject "TextReplacement" $ \o ->
    TextReplacement
      <$> o
        Aeson..: "range"
      <*> o
        Aeson..: "description"
      <*> o
        Aeson..: "replacementText"
      <*> o
        Aeson..: "fileUri"

-- | Computes code actions for a document.
executeCommandHandler :: Msg.TRequestMessage 'Msg.Method_WorkspaceExecuteCommand -> (Either Msg.ResponseError (Aeson.Value |? Null) -> Lsp ()) -> Lsp ()
executeCommandHandler m respond =
  respond =<< runExceptT do
    let cmd = m ^. params . command
    let args = m ^. params . arguments
    let invalidCmdErr = throwError $ Msg.ResponseError (InR ErrorCodes_InvalidParams) "Invalid command" Nothing
    _ <-
      case cmd of
        "replaceText" -> case args of
          Just [Aeson.fromJSON -> Aeson.Success (TextReplacement range description replacementText fileUri)] -> do
            let params =
                  ApplyWorkspaceEditParams
                    (Just description)
                    (WorkspaceEdit (Just ((Map.singleton fileUri [TextEdit range replacementText]))) Nothing Nothing)
            lift
              ( sendRequest Msg.SMethod_WorkspaceApplyEdit params $ \case
                  Left err -> Debug.debugM Debug.LSP "Error applying workspace edit" err
                  Right _ -> pure ()
              )
          _ -> invalidCmdErr
        _ -> invalidCmdErr
    pure $ InL Aeson.Null
