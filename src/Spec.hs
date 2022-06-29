module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

  describe "prueba de f" $ do
    it "para numeros grandes" $ do
      f 6 `shouldBe` 6
    it "para numeros intermedios -valor limite inferior" $ do
      f 0 `shouldBe` (-1)
    it "para numeros intermedios -valor limite superior" $ do
      f 5 `shouldBe` 4
    it "para numeros negativos" $ do
      f (-2) `shouldBe` (-1)

-- Tests Kata 1

  describe "prueba de funcionLoca" $ do
    it "para numeros impares" $ do
      funcionLoca 7 "_" `shouldBe` 7
    it "cuando el largo de la palabra es mayor al numero" $ do
      funcionLoca 2 "star" `shouldBe` 4
    it "cuando el largo de la palabra es igual al numero" $ do
      funcionLoca 4 "star" `shouldBe` 0
    it "cuando el largo de la palabra es menor al numero" $ do
      funcionLoca 6 "star" `shouldBe` 2