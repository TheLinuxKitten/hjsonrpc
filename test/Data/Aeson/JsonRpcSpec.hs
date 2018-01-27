{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.JsonRpcSpec (main, spec) where

import Data.Aeson.JsonRpc
import Data.Aeson.Types
import Data.Either (isLeft)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "JsonRpc" $ do
    let mName = "method"
    it "Codificar y decodificar Requests" $ do
      let rq1 = JsonRpcRequest JsonRpcV2 mName Null (JsonRpcIdInt 1)
      jsonRpcDecodeRequest (jsonRpcEncodeRequest rq1) `shouldBe` Right rq1
      let rqVal2 = String "Value"
      let rq2 = JsonRpcRequest JsonRpcV2 mName rqVal2 (JsonRpcIdTxt "id-2")
      jsonRpcDecodeRequest (jsonRpcEncodeRequest rq2) `shouldBe` Right rq2
      let rq3 = JsonRpcRequest JsonRpcV1 mName Null (JsonRpcIdInt 5)
      let rs3 = JsonRpcRequest JsonRpcV1 mName emptyArray (JsonRpcIdInt 5)
      jsonRpcDecodeRequest (jsonRpcEncodeRequest rq3) `shouldBe` Right rs3
      let rq4 = JsonRpcRequest JsonRpcV1 mName rqVal2 (JsonRpcIdTxt "id-6")
      jsonRpcDecodeRequest (jsonRpcEncodeRequest rq4) `shouldBe` Right rq4
    it "Codificar y decodificar Responses" $ do
      jsonRpcDecodeResponse "" `shouldSatisfy` isLeft
      jsonRpcDecodeResponse "{\"error\":\"error\"}" `shouldSatisfy` isLeft
      jsonRpcDecodeResponse "{\"id\":1,\"jsonrpc\":\"2.0\",\"error\":\"error\"}" `shouldSatisfy` isLeft
      jsonRpcDecodeResponse "{\"id\":1,\"jsonrpc\":\"1.0\",\"result\":null}" `shouldSatisfy` isLeft
      jsonRpcDecodeResponse "{\"id\":1,\"jsonrpc\":\"1.0\",\"result\":null,\"error\":null}" `shouldSatisfy` isLeft
      jsonRpcDecodeResponse "{\"jsonrpc\":\"2.0\",\"result\":null}" `shouldSatisfy` isLeft
      let rs5 = JsonRpcResponseResult JsonRpcV1 (String "texto") (JsonRpcIdTxt "id-1")
      jsonRpcDecodeResponse "{\"id\":\"id-1\",\"jsonrpc\":\"1.0\",\"result\":\"texto\",\"error\":null}" `shouldBe` Right rs5
      let rs6 = JsonRpcResponseError JsonRpcV1 (JsonRpcErrorValue (Object (HM.fromList [("err0",String "err0"),("err1",String "err1")]))) (JsonRpcIdTxt "id-1")
      jsonRpcDecodeResponse "{\"id\":\"id-1\",\"jsonrpc\":\"1.0\",\"error\":{\"err0\":\"err0\",\"err1\":\"err1\"}}" `shouldBe` Right rs6
      let rs7 = JsonRpcOrphanError JsonRpcV1 (JsonRpcErrorValue (Object (HM.fromList [("err0",String "err0"),("err1",String "err1")])))
      jsonRpcDecodeResponse "{\"jsonrpc\":\"1.0\",\"error\":{\"err0\":\"err0\",\"err1\":\"err1\"}}" `shouldBe` Right rs7
      let rs8 = JsonRpcResponseError JsonRpcV1 (JsonRpcErrorObject 2300 "message" (Object (HM.fromList [("err0",String "err0"),("err1",String "err1")]))) (JsonRpcIdTxt "id-1")
      jsonRpcDecodeResponse "{\"id\":\"id-1\",\"jsonrpc\":\"1.0\",\"error\":{\"code\":2300,\"message\":\"message\",\"data\":{\"err0\":\"err0\",\"err1\":\"err1\"}}}" `shouldBe` Right rs8
      jsonRpcDecodeResponse (jsonRpcEncodeResponse rs5) `shouldBe` Right rs5
      jsonRpcDecodeResponse (jsonRpcEncodeResponse rs6) `shouldBe` Right rs6
      jsonRpcDecodeResponse (jsonRpcEncodeResponse rs7) `shouldBe` Right rs7
      jsonRpcDecodeResponse (jsonRpcEncodeResponse rs8) `shouldBe` Right rs8

