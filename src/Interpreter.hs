module Interpreter
  (
    lambdaEval,
    byValue,
    byName,

    Context,
    evalStrategy,

    Strategy,
    evalBodies,

    defaultContext
  )
where

import qualified Data.Map.Strict as Map
import LambdaAST

data Strategy = Strategy {
  prepareArgument :: Context -> LambdaTerm -> (Context, LambdaTerm),
  evalBodies :: Bool}

type Environment = Map.Map String LambdaTerm

data Context = Context {
  evalStrategy :: Strategy,
  environment :: Environment}

instance Show Context where
  show context = show $ Map.assocs $ environment context

bind :: Context -> String -> LambdaTerm -> Context
bind context var value =
  context {environment = Map.insert var value $ environment context}

varLookup :: Context -> String -> LambdaTerm
varLookup context var = case Map.lookup var $ environment context of
  Nothing -> Variable var
  (Just term) -> term

emptyEnvironment = Map.empty

lambdaEval :: Context -> LambdaTerm -> (Context, LambdaTerm)
lambdaEval context@Context {evalStrategy = Strategy {evalBodies = evalBodies}}
  l@(Lambda parameter term) =
  if evalBodies
  then
    let (context', term') = lambdaEval context term in
      (context', Lambda parameter term')
  else
    (context, l)
lambdaEval context a@(Application function argument) =
  lambdaApply context function argument
lambdaEval context (Variable var) =
  case varLookup context var of
    v@(Variable var') ->
      if var' == var
      then (context, v)
      else lambdaEval context v
    term -> lambdaEval context term
lambdaEval context (Definition name value) =
  (bind context name value, value)

lambdaApply :: Context -> LambdaTerm -> LambdaTerm -> (Context, LambdaTerm)
lambdaApply context (Lambda parameter term) argument =
  let (context', argument') =
        prepareArgument (evalStrategy context) context argument in
    lambdaEval context' (replaceWithSubtree parameter argument' term)
lambdaApply context function argument =
  let (context', function') = lambdaEval context function in
    if function == function'
    then
      let (context'', argument') = lambdaEval context' argument in
        (context'', Application function' argument')
    else lambdaApply context' function' argument

defaultContext = Context
  {
    evalStrategy = byName,
    environment = emptyEnvironment
  }

byValue = Strategy
          {
            prepareArgument = prepareArgument,
            evalBodies = False
          }
  where
    prepareArgument :: Context -> LambdaTerm -> (Context, LambdaTerm)
    prepareArgument context argument = lambdaEval context argument

byName = Strategy
         {
           prepareArgument = prepareArgument,
           evalBodies = False
         }
  where
    prepareArgument :: Context -> LambdaTerm -> (Context, LambdaTerm)
    prepareArgument context argument = (context, argument)
