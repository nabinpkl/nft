{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nft (
mintingNFTScriptShortBs,
nftMintingScript

)
where

import           Data.Maybe             as M
import           Data.Aeson             as A
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Codec.Serialise
import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.Short  as SBS
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

data NFTMintingParameters = NFTMintingParameters {
    name :: TokenName,
    utxoId :: TxId,
    utxoIndex :: Integer
}

PlutusTx.makeLift ''NFTMintingParameters

{-# INLINABLE mkPolicy #-}
mkPolicy :: NFTMintingParameters -> () -> ScriptContext -> Bool
mkPolicy NFTMintingParameters {..} _ ctx = 
                          traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    utxoRef :: TxOutRef
    utxoRef = TxOutRef utxoId utxoIndex

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxoRef) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(cs, tn, amt)] -> cs  == ownCurrencySymbol ctx && tn == name && amt == 1
        _                -> False

policy :: NFTMintingParameters -> Scripts.MintingPolicy
policy params = mkMintingPolicyScript 
    ($$(PlutusTx.compile [|| \p -> Scripts.wrapMintingPolicy (mkPolicy p) ||]) 
    `PlutusTx.applyCode` 
    PlutusTx.liftCode params)


plutusScript :: Script
plutusScript =
  unMintingPolicyScript (policy parameters)
    where 
        parameters = NFTMintingParameters {
            utxoId = M.fromJust $ A.decode $ "{\"getTxId\" : \"96667140a738478f579a640a90c76a18d7c4d9d0d3cadb2e7860f82e9ddda584\"}",
            utxoIndex = 0,
            name = TokenName "OlgaNFT"
            }

validator :: Validator
validator = Validator $ plutusScript

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

nftMintingScript :: PlutusScript PlutusScriptV1
nftMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingNFTScriptShortBs :: SBS.ShortByteString
mintingNFTScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor