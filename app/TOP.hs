-- | Transducers-based One-dimensional Polymorphic Parser,
-- | also known as "TOP Parser".
-- |
-- | Provides a generic means to parse arbitrary one-dimensional
-- | input given a determenistic transducer (state machine with output)
-- | for the corresponding alphabet.
--
-- Note: the transducer, in fact, does not necessairly have to be a state
-- machine. For example, one could easily solve the braces pairs evaluation
-- problem with `TOP.parse`, using lists as states (that would effectively be
-- a pushdown automaton).
module TOP (
   parse,
) where

-- | Parse a sequence of tokens with a transducer represented with initial state,
-- | final state checker, and transition function.
-- |
-- | Transition function, given a state and a token, should return `Nothing` if
-- | the transition is invalid (which fails the parse); `Just (nextState, Nothing)`
-- | to make transition to `nextState` without producing any output;
-- | `Just (nextState, Just nextCharacter)` to proceed to `nextState` and output
-- | `nextCharacter`.
--
-- The parsing may fail either due to an invalid transition or if the input is empty
-- when the transducer is in a non-final state.
-- If the parsing fails due to an invalid transition, `Left (context, Just token)` is
-- returned, where `context` is the sequence of tokens since last final state, `token` is
-- the token that failed the parsing (caused the invalid transition).
-- If the parsing fails at the end of input, `Left (context, Nothing)` is returned
-- (`context` has the same meaning as above).
--
-- If the parsing succeeded, `Right outputList` is returned, where `outputList` is the
-- list of all outputs produced by the transducer during the parsing process.
parse
    :: [token]          -- ^ What to parse
    -> state            -- ^ Initial state
    -> (state -> Bool)  -- ^ Check if the state is final
    -> (state -> token -> Maybe (state, Maybe output))  -- ^ Transition function
    -> Either ([token], Maybe token) [output]
parse tokens initS isFinal transition =
    case parse' tokens initS isFinal transition of
         Left (context, worstToken, _internal) -> Left (context, worstToken)
         Right output -> Right output


-- | Same as `parse` but with additional info in case of error
-- | (that is purely internal and is not useful for the users of `parse`).
--
-- The third argument of the `Left` return value tells when to stop
-- expanding the context (after a final state is found when tracing back).
parse'
    :: [token]
    -> state
    -> (state -> Bool)
    -> (state -> token -> Maybe (state, Maybe output))
    -> Either ([token], Maybe token, Bool) [output]
parse' [] s isFinal _
    | isFinal s = Right []
    | otherwise = Left ([], Nothing, False)
parse' (tok:toks) s isFinal transition =
    case transition s tok of
         Nothing             -> Left ([], Just tok, isFinal s)
         Just (nextS, mbOut) ->
             let nextRes = parse' toks nextS isFinal transition in
             case nextRes of
                  Left err -> Left (forwardError err)
                  Right output -> Right (addIfAny output mbOut)
    where
        addIfAny :: [a] -> Maybe a -> [a]
        addIfAny l Nothing = l
        addIfAny l (Just x) = x:l

        forwardError (badToks, worstTok, dontForward)
            | dontForward = (badToks, worstTok, True)
            | otherwise   = (tok:badToks, worstTok, isFinal s)
