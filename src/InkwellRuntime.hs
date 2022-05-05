{-# LANGUAGE OverloadedStrings, DeriveGeneric, BlockArguments, FlexibleInstances #-}

module InkwellRuntime
    ( continueStory
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Map hiding (toList)
import Data.Text hiding (init, last)
import Data.Scientific
import Data.Vector as V (init, last)
import GHC.Generics
import Data.Traversable (traverse)
import Data.Foldable (toList)
import Control.Applicative

data RootContainer = RootContainer {
                            inkVersion :: Int,
                            root:: Instruction,
                            listDefs :: Map Text (Map Text Int)
                    } deriving (Generic)
instance FromJSON RootContainer

type ContainerName = String

data CC = EvalStart | EvalOutput | EvalEnd | Duplicate | PopEvaluatedValue | PopFunction | PopTunnel
                        | BeginString | EndString | NoOp | ChoiceCount | Turns | TurnsSince | ReadCount
                        | Random | SeedRandom | VisitIndex | SequenceShuffleIndex | StartThread | Done
                        | End | ListFromInt | ListRange | ListRandom
    deriving (Show)

data NF = Add | Subtract | Divide | Multiply | Mod | Negate | Equal | Greater | Less | GreaterThanOrEquals
                        | LessThanOrEquals | NotEquals | Not | And | Or | Min | Max | Pow | Floor | Ceiling | Int
                        | Float | Has  | Hasnt | Intersect | ListMin | ListMax | All | Count | ValueOfList | Invert
    deriving (Show)

data Childrens = Empty
    deriving (Show)

data Instruction = ControlCommand CC
                 | NativeFunction NF
                 | StringValue String
                 | NewLine
                 | NumberValue Scientific
                 | Divert
                 | Container [Instruction] --ContainerChildren
                 | NamedContainer ContainerName [Instruction] --ContainerChildren
                 | ContainerMeta Childrens
                 | None
    deriving (Show)

parseControlCommand = withText "Instruction" f
    where -- ControlCommand
          f "ev" = return $ ControlCommand EvalStart
          f "out" = return $ ControlCommand EvalOutput
          f "/ev" = return $ ControlCommand EvalEnd
          f "du" = return $ ControlCommand Duplicate
          f "pop" = return $ ControlCommand PopEvaluatedValue
          f "~ret" = return $ ControlCommand PopFunction
          f "->->" = return $ ControlCommand PopTunnel
          f "str" = return $ ControlCommand BeginString
          f "/str" = return $ ControlCommand EndString
          f "nop" = return $ ControlCommand NoOp
          f "choiceCnt" = return $ ControlCommand ChoiceCount
          f "turn" = return $ ControlCommand Turns
          f "turs" = return $ ControlCommand TurnsSince
          f "readc" = return $ ControlCommand ReadCount
          f "rnd" = return $ ControlCommand Random
          f "srnd" = return $ ControlCommand SeedRandom
          f "visit" = return $ ControlCommand VisitIndex
          f "seq" = return $ ControlCommand SequenceShuffleIndex
          f "thread" = return $ ControlCommand StartThread
          f "done" = return $ ControlCommand Done
          f "end" = return $ ControlCommand End
          f "listInt" = return $ ControlCommand ListFromInt
          f "range" = return $ ControlCommand ListRange
          f "lrnd" = return $ ControlCommand ListRandom
          f _ = fail "not a ControlCommand"

parseNativeFunction = withText "Instruction" f
    where -- NativeFunction
          f "+" = return $ NativeFunction Add
          f "-" = return $ NativeFunction Subtract
          f "/" = return $ NativeFunction Divide
          f "*" = return $ NativeFunction Multiply
          f "%" = return $ NativeFunction Mod
          f "_" = return $ NativeFunction Negate
          f "==" = return $ NativeFunction Equal
          f ">" = return $ NativeFunction Greater
          f "<" = return $ NativeFunction Less
          f ">=" = return $ NativeFunction GreaterThanOrEquals
          f "<=" = return $ NativeFunction LessThanOrEquals
          f "!=" = return $ NativeFunction NotEquals
          f "!" = return $ NativeFunction Not
          f "&&" = return $ NativeFunction And
          f "||" = return $ NativeFunction Or
          f "MIN" = return $ NativeFunction Min
          f "MAX" = return $ NativeFunction Max
          f "POW" = return $ NativeFunction Pow
          f "FLOOR" = return $ NativeFunction Floor
          f "CEILING" = return $ NativeFunction Ceiling
          f "INT" = return $ NativeFunction Int
          f "FLOAT" = return $ NativeFunction Float
          f "?" = return $ NativeFunction Has
          f "!?" = return $ NativeFunction Hasnt
          f "L^" = return $ NativeFunction Intersect
          f "LIST_MIN" = return $ NativeFunction ListMin
          f "LIST_MAX" = return $ NativeFunction ListMax
          f "LIST_ALL" = return $ NativeFunction All
          f "LIST_COUNT" = return $ NativeFunction Count
          f "LIST_VALUE" = return $ NativeFunction ValueOfList
          f "LIST_INVERT" = return $ NativeFunction Invert
          f _ = fail "not a ControlCommand"

parseStringValue = withText "Instruction" f
    where -- StringValue
          f t = case s of
                    '\n' -> return $ NewLine
                    '^' -> return $ StringValue xs
                    _ -> fail "Unrecognized string"
                where s:xs = unpack t

parseNumber (Number n) = return $ NumberValue n
parseNumber _ = fail "Unrecognized number"

parseDivert = withObject "Instruction" f
    where -- Divert
        f t = return Divert

parseChild = withObject "ContainerChildren" $ \obj -> do
    maybeName <- obj .:? "#n"
    case maybeName of
        Just name -> pure $ NamedContainer name
        _ -> pure Container

parseArray = withArray "Container" f
    where
        f t =
            let start = V.init t
                meta = V.last t
                container = Container
            in  (fmap (container . toList) . traverse parseJSON) start



parseNull Null = return None
parseNull _ = fail "failed to parse"

instance FromJSON Instruction where
    parseJSON v =
            parseControlCommand v
        <|> parseNativeFunction v
        <|> parseStringValue v
        <|> parseNumber v
        <|> parseDivert v
        <|> parseArray v
        <|> parseNull v


continueStory :: IO ()
continueStory = do
    putStrLn "inkwell 0.0"
    -- let storyJson = "{ \"inkVersion\": 20, \"root\":[\"ev\", 2,\"\\n\", \"^miaou\",\"+\",\"/ev\", [2,2,\"-\", null], null], \"listDefs\": {} }"
    let storyJson = "{ \"inkVersion\": 20, \"root\":[\"^Hello world\", {\"#n\": \"hello\"}, {}], \"listDefs\": {} }"
    -- let storyJson = "{ \"inkVersion\": 20, \"root\":[\"^Hello world\", 2], \"listDefs\": {} }"
    let maybeStory = eitherDecode storyJson :: Either String RootContainer


    case maybeStory of
            Right story -> do
                putStrLn "parsed"
                putStrLn $ "Your story has version " ++ show (inkVersion story)
                print (root story)
            Left  e -> do
                putStrLn "Could not parse inkjson"
                fail $ "while decoding a Bar: " ++ e
                return ()
