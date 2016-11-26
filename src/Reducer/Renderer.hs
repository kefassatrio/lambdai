module Reducer.Renderer
  (
    render,

    RendererSpec (RendererSpec),
    clRendererSpec
  )
where

import Prelude hiding (Left, Right)

import LambdaAST
import LambdaAST.Path
import Reducer.Step

type Renderer = Trace -> String

data RendererSpec = RendererSpec { therefore :: String,
                                   beta :: String,
                                   delta :: String,
                                   lambda :: String,
                                   firstLinePrefix :: String,
                                   linePrefix :: String,
                                   lineEnd :: String,
                                   underlineBegin :: String,
                                   underlineEnd :: String,
                                   openParen :: String,
                                   closeParen :: String,
                                   period :: String,
                                   application :: String,
                                   definition :: String,
                                   alignStep :: String -> String }

consoleWidth = 80

clRendererSpec = RendererSpec
  {
    therefore = "∴",
    beta = "β",
    delta = "δ",
    lambda = "λ",
    firstLinePrefix = "  ",
    linePrefix = "= ",
    lineEnd = "\n",
    underlineBegin = "\x1B[4m",
    underlineEnd = "\x1B[0m",
    openParen = "(",
    closeParen = ")",
    period = ".",
    application = " ",
    definition = " = ",
    alignStep = alignStep
  }
  where
    alignStep :: String -> String
    alignStep s | '\x1B' `elem` s = alignLeft (consoleWidth + 3) s ++ " "
                | otherwise = alignLeft (consoleWidth - 5) s ++ " "

render :: RendererSpec -> Renderer
render s (Trace init []) =
  renderTerm s init
render s (Trace init (step:steps)) =
  firstLinePrefix s ++
  renderTermWithHighlight (Just $ path step) s init ++
  lineEnd s ++
  renderSteps s (reductionType s step) (Trace (newTerm step) steps)

renderSteps :: RendererSpec -> String -> Trace -> String
renderSteps s rt (Trace init []) =
  linePrefix s ++ renderStep s rt init (NoReduction init)
renderSteps s rt (Trace init (step:steps)) =
  linePrefix s ++ renderStep s rt init step ++ lineEnd s ++
  renderSteps s (reductionType s step) (Trace (newTerm step) steps)

renderStep :: RendererSpec -> String -> LambdaTerm -> ReductionStep -> String
renderStep s rt t NoReduction {} =
  (alignStep s $ renderTerm s t) ++ therefore s ++ rt
renderStep s rt t step =
  (alignStep s $ renderTermWithHighlight (Just $ path step) s t) ++ therefore s ++ rt

reductionType :: RendererSpec -> ReductionStep -> String
reductionType s Beta {} = beta s
reductionType s Delta {} = delta s
reductionType _ _ = ""

renderTerm :: RendererSpec -> LambdaTerm -> String
renderTerm = renderTermWithHighlight Nothing

renderTermWithHighlight :: Maybe Path -> RendererSpec -> LambdaTerm -> String
renderTermWithHighlight (Just (Path [])) s t = underlineBegin s ++ renderTerm s t ++ underlineEnd s
renderTermWithHighlight p s (Lambda parameter term) =
  openParen s ++ lambda s ++ parameter ++ period s ++
  renderTermWithHighlight (descend p Right) s term ++ closeParen s
renderTermWithHighlight p s (Application function argument) =
  openParen s ++ renderTermWithHighlight (descend p Left) s function ++ application s ++
  renderTermWithHighlight (descend p Right) s argument ++ closeParen s
renderTermWithHighlight p s (Variable var) = var
renderTermWithHighlight p s (Definition name value) = name ++ definition s ++
  renderTermWithHighlight (descend p Right) s value

alignLeft :: Int -> String -> String
alignLeft n s = s ++ replicate (n - (length s)) ' '
