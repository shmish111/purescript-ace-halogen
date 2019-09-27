module Ace.Halogen.Component
  ( aceComponent
  , AceQuery(..)
  , AceMessage(..)
  , Autocomplete(..)
  , CompleteFn
  ) where

import Prelude

import Ace as Ace
import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Ext.LanguageTools as LanguageTools
import Ace.Ext.LanguageTools.Completer as Completer
import Ace.Types (Editor, Completion, Position, EditSession)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, runAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Now (now)
import Effect.Random (random)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen (Component, HalogenM, RefLabel(..), getHTMLElementRef, gets, lift, liftEffect, mkComponent, mkEval, put, raise, subscribe) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (Emitter(..), Finalizer(..))
import Halogen.Query.EventSource (effectEventSource) as H
import Web.HTML.HTMLElement (HTMLElement)

-- | Effectful knot of autocomplete functions. It's needed because
-- | `languageTools.addCompleter` is global and adds completer to
-- | all editors
foreign import completeFns ∷ ∀ eff. Ref (Object (CompleteFn eff))

-- | This flag is used to determine if `languageTools` initialized
foreign import initialized ∷ Ref Boolean

-- | Global key of currently focused component. Used only to take
-- | autocomplete function
foreign import focused ∷ Ref String

-- | Get `dataset` property of element
foreign import dataset
  ∷ HTMLElement
  → Effect (Object String)

-- | Take completion function for currently selected component
completeFnFocused ∷ ∀ eff. Effect (CompleteFn eff)
completeFnFocused = do
  focusedKey ← Ref.read focused
  mFns ← Ref.read completeFns
  maybe (pure emptyCompleteFn) pure $ Object.lookup focusedKey mFns
  where
  emptyCompleteFn ∷ CompleteFn eff
  emptyCompleteFn _ _ _ _ = pure []

-- | Set autocomplete resume
setAutocompleteResume
  ∷ Maybe Autocomplete → Editor → Effect Unit
setAutocompleteResume Nothing editor = do
  Editor.setEnableBasicAutocompletion false editor
  Editor.setEnableLiveAutocompletion false editor
setAutocompleteResume (Just Basic) editor = do
  Editor.setEnableLiveAutocompletion false editor
  Editor.setEnableBasicAutocompletion true editor
setAutocompleteResume (Just Live) editor = do
  Editor.setEnableLiveAutocompletion true editor
  Editor.setEnableBasicAutocompletion true editor

-- | Language tools and autocomplete initializer. Runs once.
enableAutocomplete ∷ Effect Unit
enableAutocomplete = do
  languageToolsInitialized ← Ref.read initialized
  when (not languageToolsInitialized) do
    completer ← Completer.mkCompleter globalCompleteFn
    tools ← LanguageTools.languageTools
    LanguageTools.addCompleter completer tools
    Ref.write true initialized
  where
  globalCompleteFn editor session position prefix cb = do
    fn ← completeFnFocused
    void
      $ runAff (either (const (pure unit)) (cb <<< Just))
      $ fn editor session position prefix

-- | Generate unique key for component
genKey ∷ Effect String
genKey = do
  rn1 ← random
  rn2 ← random
  instant ← now
  pure $ show rn1 <> show (unwrap (unInstant instant)) <> show rn2

data Autocomplete = Live | Basic

-- | Ace query algebra
-- | - `SetElement` - used to capture a reference to the component's element
-- | - `Init` - used internally to handle initialization of component
-- | - `Quit` - used internally to handle finalizing of component.
-- | - `GetText` - gets the current text value
-- | - `SetText` - alters the current text value
-- | - `SetAutocomplete` - sets autocomplete resume:
-- |   - `Nothing` - turns it off
-- |   - `Just Basic` - enables basic autocompletions (triggered by `Alt + Space` or `Ctrl + Space`)
-- |   - `Just Live` - enables live autocomplete
-- | - `SetCompleteFn` - sets function providing autocomplete variants.
-- | - `GetEditor` - returns ace editor instance handled by this component.
data AceQuery a
  = GetText (String → a)
  | SetText String a
  | SetAutocomplete (Maybe Autocomplete) a
  | SetCompleteFn (∀ eff. CompleteFn eff) a
  | GetEditor (Maybe Editor → a)

type AceChange =
     { action :: String
     , end :: { column :: Int
              , row :: Int
              }
     , lines :: Array String
     , start :: { column :: Int
                , row :: Int
                }
     }

data AceAction
  = Init
  | Quit
  | HandleChange AceChange

-- | Ace output messages
-- | - `AceValueChanged` - raised when the value in the editor is changed.
data AceMessage = TextChanged String

-- | The type for autocomplete function s. Takes editor, session, text position,
-- | prefix, and returns array of possible completions in the `Aff` monad.
type CompleteFn eff
  = Editor
  → EditSession
  → Position
  → String
  → Aff (Array Completion)

-- | Ace component state.
-- | - `key` - unique key of this instance
-- | - `editor` - Ace editor instance wrapped by this component
type AceState =
  { key ∷ Maybe String
  , editor ∷ Maybe Editor
  }

type DSL slots m = H.HalogenM AceState AceAction slots AceMessage m

-- | An initial empty state value.
initialState ∷ AceState
initialState =
  { key: Nothing
  , editor: Nothing
  }

-- | The Ace component.
aceComponent
  ∷ ∀ m
  . MonadAff m
  ⇒ (Editor → m Unit)
  → Maybe Autocomplete
  → H.Component HH.HTML AceQuery Unit AceMessage m
aceComponent setup resume =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval
                { handleAction: handleAction setup resume
                , handleQuery
                , initialize: Just Init
                , finalize: Just Quit
                , receive: const Nothing
                }
    }

render ∷ forall p i. AceState → HH.HTML p i
render = const $ HH.div [ HP.ref (H.RefLabel "container") ] []

handleQuery ∷ ∀ slots m a
 . MonadAff m
 ⇒ AceQuery a
 -> H.HalogenM AceState AceAction slots AceMessage m (Maybe a)
handleQuery = case _ of
  GetEditor k → do
    r <- H.gets _.editor
    pure $ Just $ k r

  GetText k → do
    s <- getText
    pure $ Just $ k s

  SetText text next → do
    H.gets _.editor
      >>= traverse_ \editor → do
        current ← H.liftEffect $ Editor.getValue editor
        when (text /= current) $ void
          $ H.liftEffect (Editor.setValue text Nothing editor)
    pure $ Just next

  SetAutocomplete mbAc next → do
    H.gets _.editor
      >>= traverse_ (H.liftEffect <<< setAutocompleteResume mbAc)
    pure $ Just next

  SetCompleteFn fn next → do
    H.gets _.key
      >>= traverse_ \key →
      H.liftEffect $ Ref.modify (Object.insert key fn) completeFns
    pure $ Just next

handleAction ∷ ∀ slots m
 . MonadAff m
 ⇒ (Editor → m Unit)
 → Maybe Autocomplete
 → AceAction
 -> H.HalogenM AceState AceAction slots AceMessage m Unit
handleAction setup resume = case _ of
  Init → do
    H.getHTMLElementRef (H.RefLabel "container") >>= traverse_ \el → do
      key ← H.gets _.key >>= maybe (H.liftEffect genKey) pure
      editor ← H.liftEffect $ Ace.editNode el Ace.ace
      H.put { key: Just key, editor: Just editor }
      H.liftEffect do
        enableAutocomplete
        setAutocompleteResume resume editor
        Editor.onFocus editor $ Ref.write key focused
      session ← H.liftEffect $ Editor.getSession editor
      subscriptionId <- H.subscribe $ H.effectEventSource
         (\(Emitter emitter) -> do Session.onChange session (emitter <<< Left <<< HandleChange)
                                   pure $ Finalizer $ pure unit)
      H.lift $ setup editor

  HandleChange k → do
    H.raise <<< TextChanged =<< getText

  Quit → do
    H.gets _.key
      >>= traverse_ \key →
      H.liftEffect $ Ref.modify (Object.delete key) completeFns


getText :: forall slots output m. MonadEffect m => H.HalogenM AceState AceAction slots output m String
getText =
    maybe (pure "") (H.liftEffect <<< Editor.getValue) =<< H.gets _.editor
