module LogAnalysisSpec
  ( spec
  ) where

import           Test.Hspec

import           LogAnalysis
import           LogAnalysis.Internal

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "should parse a string to message" $ do
      parseMessage "E 2 562 help help" `shouldBe`
        LogMessage (Error 2) 562 "help help"
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      parseMessage "This is not in the right format" `shouldBe`
        Unknown "This is not in the right format"

  describe "parse" $ do
    it "should parse multi-line string of messages" $ do
      parse
        (unlines
           [ "I 5053 pci_id: con ing!"
           , "I 4681 ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
           , "W 3654 e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
           , "I 4076 verse.'"
           , "I 4764 He trusts to you to set them free,"
           , "I 858 your pocket?' he went on, turning to Alice."
           ]) `shouldBe`
        [ LogMessage Info 5053 "pci_id: con ing!"
        , LogMessage
            Info
            4681
            "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
        , LogMessage
            Warning
            3654
            "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
        , LogMessage Info 4076 "verse.'"
        , LogMessage Info 4764 "He trusts to you to set them free,"
        , LogMessage Info 858 "your pocket?' he went on, turning to Alice."
        ]

  describe "insert" $ do
    it "should not insert Unkown messages" $
      insert (Unknown "lol") (Node Leaf (LogMessage Info 3 "lol") Leaf) `shouldBe`
      (Node Leaf (LogMessage Info 3 "lol") Leaf)
    it "should insert message into empty tree" $
      insert (LogMessage Info 5 "lol") Leaf `shouldBe`
      Node Leaf (LogMessage Info 5 "lol") Leaf
    it "should insert smaller timestamp message into left sub-tree" $
      insert
        (LogMessage Info 1 "lol")
        (Node Leaf (LogMessage Info 5 "lol") Leaf) `shouldBe`
      Node
        (Node Leaf (LogMessage Info 1 "lol") Leaf)
        (LogMessage Info 5 "lol")
        Leaf
    it "should insert bigger timestamp message into right sub-tree" $
      insert
        (LogMessage Info 9 "lol")
        (Node Leaf (LogMessage Info 5 "lol") Leaf) `shouldBe`
      Node
        Leaf
        (LogMessage Info 5 "lol")
        (Node Leaf (LogMessage Info 9 "lol") Leaf)

  describe "build" $ do
    it "should build a correct MessageTree from a list of LogMessages" $
      build
        [ LogMessage Info 5053 "pci_id: con ing!"
        , LogMessage
            Info
            4681
            "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
        , LogMessage
            Warning
            3654
            "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
        , LogMessage Info 4076 "verse.'"
        , LogMessage Info 4764 "He trusts to you to set them free,"
        , LogMessage Info 858 "your pocket?' he went on, turning to Alice."
        ] `shouldBe`
      Node
        (Node
           (Node
              (Node
                 Leaf
                 (LogMessage
                    Info
                    858
                    "your pocket?' he went on, turning to Alice.")
                 Leaf)
              (LogMessage
                 Warning
                 3654
                 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled")
              (Node Leaf (LogMessage Info 4076 "verse.'") Leaf))
           (LogMessage
              Info
              4681
              "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)")
           (Node
              Leaf
              (LogMessage Info 4764 "He trusts to you to set them free,")
              Leaf))
        (LogMessage Info 5053 "pci_id: con ing!")
        Leaf
