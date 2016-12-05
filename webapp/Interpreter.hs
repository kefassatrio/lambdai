{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpreter
  (
    module Reducer,

    InterpreterRequest(..),
    emptyRequest,
    maxEvalStepLimit,

    InterpreterResponse(..),
    evalLines,

    webContext,
    chooseStrategy,

    render,
    latexRendererSpec
  )
where

import Parser
import Reducer
import Reducer.Step
import Reducer.Renderer

data InterpreterRequest = InterpreterRequest { context :: Context,
                                               input :: [String]} |
                          EmptyRequest

newtype InterpreterResponse = InterpreterResponse (Either String Trace)

emptyRequest :: InterpreterRequest
emptyRequest = InterpreterRequest webContext []

maxEvalStepLimit :: Int
maxEvalStepLimit = 3000

webContext :: Context
webContext = defaultContext
  {
    renderer = latexRendererSpec,
    evalStepLimit = 200
  }

chooseStrategy :: String -> Strategy
chooseStrategy = \case
  "normal order" -> normalOrder
  "applicative order" -> applicativeOrder
  "call by name" -> callByName
  "call by value" -> callByValue
  _ -> strategy defaultContext

evalLines :: Context -> [String] -> [InterpreterResponse]
evalLines c@Context
  {
    evalStepLimit = n,
    strategy = s,
    table = dt
  } = \case
  [] -> []
  (l:ls) ->
    case lambdaRead l of
      Left e -> [InterpreterResponse $ Left $ show e]
      Right term ->
        let (dt', trace) = reduceToNormalForm s dt term in
          (InterpreterResponse $
           Right $ limitSteps n trace) : evalLines c { table = dt' } ls
