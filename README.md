# Miso

A tasty haskell web framework

---

### Motivation
 - Understand virtual DOM
 - Implement efficient diffing
 - Easy-to-use web framework
   - Like Elm, but with typeclasses, better FFI
---

### Philosophy
 - Resist the urge for premature abstraction
  - Free Monad DSL for events, runs into the expression problem
  - If you don’t know if its needed, don’t use it
  - Consider dependency implications
  - Consider build problems / issues

---

### Philosophy
  - Keep the view pure
    - `IO` in view violates separation of concerns
    - Produces spaghetti code

---

####  What is the Elm architecture?

---

###  Model

```haskell
-- Eq instance required
type Model = Int
```

---

###  Update

```haskell
-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  deriving (Show, Eq)

-- | Function to update model based on Actions
-- with optional effects
updateModel :: Action -> Model -> Effect Action Model
updateModel SubtractOne m = pure (m - 1)
updateModel AddOne m      = pure (m + 1)
updateModel NoOp m        = pure m
```

---

### View

```haskell
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (toMisoString x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
```

---

### Start

```haskell
-- | Entry point for a miso application
main :: IO ()
main = startApp App { model = 0
                    , update = updateModel
                    , view = viewModel
                    , events = M.singleton "click" False
                    , mountPoint = Nothing
                    , subs = []
					}
```

---

### Event loop

```haskell
startApp :: App -> IO ()
startApp App {..} = do
  modelRef <- newIORef model
  viewRef <- newIORef (view model)
  forever $ do
    actions <- getEvents
	currentModel <- readIORef modelRef
    let newModel = foldr update currentModel actions
	    newTree = view newModel
	when (currentModel /= newModel) $ do
  	  waitForAnimationFrame
        oldTree <- readIORef viewRef
        oldTree `diff` newTree
	    writeIORef viewRef newTree
	    writeIORef modelRef newModel
```

---

### What is a virtual DOM?
  - Pure abstraction over the real DOM
  - Rose tree, with some additional properties

```haskell
data VTree a = VTree a [VTree a]
```

---

### Sample Implementation

```haskell
data VTree action where
  VNode :: { vType :: Text -- ^ Element type (i.e. "div", "a", "p")
		   , vNs :: NS -- ^ HTML or SVG
		   , vProps :: Props -- ^ Fields present on DOM Node
		   , vKey :: Maybe Key -- ^ Key used for child swap patch
		   , vChildren :: V.Vector (VTree action) -- ^ Child nodes
		   , vNode :: IORef JSVal -- ^ pointer to DOM reference
		   } -> VTree action
  VText :: { vText :: Text -- ^ TextNode content
		   , vTextNode :: IORef JSVal -- ^ pointer to DOM reference
		   } -> VTree action
  deriving Functor
```

---

### JS Virtual DOM

```javascript
var vtree = { vtype : "vnode"
            , type : "div"
            , ns : "html"
            , children : []
            , props : {}
            , ref : <DOMElement>
			}
```

---

### Attributes vs. Properties

```haskell
view = p_ [ class_ "is-active" ] [ ]
```
  - Attributes exist in HTML
  - Properties exist on DOM
  - `class` vs. `className`
  - Normalization

---

### Diffing

- Goal
  - To update the DOM as quickly as possible
  - With as few operations as possible

---

### Two-pass approach

  - React.js approach

```haskell
diffPure :: VTree action -> VTree action -> [Patches]

type Body = JSVal
patchDOM :: [Patches] -> Body -> IO ()
```

---

### Single pass approach

  - Bobril approach
  - Miso approach

```haskell
diff :: VTree action -> VTree action -> Body -> IO ()
```

---

### Null handling
- Account for tree deletion / creation
```haskell
diff :: Maybe (VTree action) -> Maybe (VTree action) -> Body -> IO ()
```

---

### Parent reference
```haskell
diff :: Maybe (VTree action) -> Maybe (VTree action) -> Parent -> IO ()
```

---

### Base case
```haskell
diff Nothing Nothing _ = pure ()
```

---

### Remove text node
```haskell
diff (Just (txt@VText{}) Nothing parent = do
  removeTextNode parent =<< readIORef (vTextNode txt)
```

---

### Additional cases
  - Remove Element node
  - Replace Text node with Element node
  - Replace Element node with Text node
  - Diff two Text nodes
  - Diff two Element nodes
  - Diff child lists

---

### Diff Children
```haskell
diff :: V.Vector (VTree a) -> V.Vector (VTree a) -> Body -> IO ()
diff = ...
```
  - Inefficient in general, usually linear in number of DOM operations.
  - Keys optimization can make this efficient

---

### Keys patch, normal way
  - Two map approach
  - Pros: clean
  - Cons: allocations

---

### The Boris Letocha optimizations

Before map construction, diff from both sides.
```haskell
  -> [ a b c ] <-
  -> [ a b c ] <-
```

Works with swapped nodes as well.
```haskell
  -> [ a b c ] <-
  -> [ c b a ] <-
```

Consruct map when nothing matches
```haskell
  -> [ e b k ] <-
  -> [ a b f ] <-
```

---

### Miso diffs in JS
 - Miso is a shallow-embedded DSL
   - [node](https://github.com/dmjio/miso/blob/master/ghcjs-src/Miso/Html/Internal.hs#L115)
 - Terms of host language translated directly to target language
   - Better for debugging
   - More control
   - More speed

---

### Can we trust?
 - [Coverage](http://coverage.haskell-miso.org)
 - [Tests](https://user-images.githubusercontent.com/875324/39086001-4298f7ea-4540-11e8-8f74-82cd2175f3aa.jpg)
 - jsverify, future verification

---

### Events
```haskell
view = button [ onClick SayHello ] [ text "Say hello" ]

-- | `onClick` defintion
onClick = onWithOptions "click" defaultOptions emptyDecoder $ \() -> SayHello
```

---

### Event decodiing
 - Turn event into JSON, [parse it in Haskell](https://github.com/dmjio/miso/blob/master/jsbits/delegate.js#L101)

```haskell
data Decoder a = Decoder {
  decoder :: Value -> Parser a -- ^ FromJSON-based Event decoder
, decodeAt :: DecodeTarget -- ^ Location in event of where to decode
}

at :: [MisoString] -> (Value -> Parser a) -> Decoder a
at decodeAt decoder = Decoder {decodeAt = DecodeTarget decodeAt, ..}

-- | Retrieves "checked" field in Decoder
checkedDecoder :: Decoder Checked
checkedDecoder = Decoder {..}
  where
    decodeAt = DecodeTarget ["target"]
    decoder = withObject "target" $ \o ->
       Checked <$> (o .: "checked")
```

---

### onWithOptions
```haskell
onWithOptions options eventName Decoder{..} toAction =
  Attribute $ \sink n -> do
   eventObj <- getProp "events" n
   eventHandlerObject@(Object eo) <- create
   jsOptions <- toJSVal options
   decodeAtVal <- toJSVal decodeAt
   cb <- asyncCallback1 $ \e -> do
       Just v <- jsvalToValue =<< objectToJSON decodeAtVal e
       case parseEither decoder v of
         Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
         Right r -> sink (toAction r)
   setProp "runEvent" (jsval cb) eventHandlerObject
   registerCallback cb
   setProp "options" jsOptions eventHandlerObject
   setProp eventName eo (Object eventObj)
```

---

### Event Delegation
  - How do we resolve our javascript events to pure `Haskell` functions?
  - We build a stack from event `target` to body
  - Index into VDOM with this stack to find pure Haskell handler
	- Annotating our VDom with an impure type is clean event delegation.
  - Adds many listeners to a single DOM node

---

### Event path
```javascript
function buildTargetToElement (element, target) {
    var stack = [];
    while (element !== target) {
      stack.unshift (target);
      target = target.parentNode;
    }
    return stack;
}
```
---

### Event batching
  - When many events flood the system
  - Folding events onto model in bulk
  - Minimize redraws

---

### Isomorphic javascript
  - Check it out https://haskell-miso.org
  - For SEO and increased page load speed.
  - Works with javascript disabled
  - Copy pointers from DOM to virtual DOM.

---

### Road map
 - Miso-native
   - Construct fully-native iOS and Android applications

---

### Thanks!
 - http://github.com/dmjio/miso
