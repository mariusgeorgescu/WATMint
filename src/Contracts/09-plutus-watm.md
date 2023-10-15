
## **Generate Issuer's Keys**
```sh
key-gen issuer
``` 

Get some TestAda from the faucet or transfer to the issuer's address from another wallet 


## **Mint Come and Go Proof Token**
Use this utxo to replace the oref in the exports of the NFT.hs file and nftCurrencySymbol of the WATMint.hs file.
```sh
utxos issuer 
``` 


Export policy
```sh
jamb -w nft
```
Save the CnGPT token representation
```sh
T=$(token -p nft jambtoken)
```

As usual, select a suitable UTXO input from issuer's address (utxos issuer) and save it to the variable U. The input should contain at least a few ADA.

We'll also need to calculate the minimum amount of ADA required for alice to mint a single unit of our asset $T. Assign the output of this command to a temporary variable MIN_U:

```sh
cardano-cli query protocol-parameters \
--out-file $PARAMS_PATH

MIN_U=$(min-utxo issuer 1 $T)
```

Build sing and sumbit minting transaction
```sh
cardano-cli transaction build \
--tx-in $U \
--tx-in-collateral $U \
--tx-out "$(addr issuer)+$MIN_U+1 $T" \
--change-address $(addr issuer) \
--mint "1 $T" \
--mint-script-file $PLUTUS_SCRIPTS_PATH/nft.plutus \
--mint-redeemer-file  $DATA_PATH/mintmode.json \
--witness-override 2 \
--out-file $TX_PATH/nft-mint.raw

tx-sign nft-mint issuer

tx-submit nft-mint
```

Check if CnGPT was minted:
```sh
utxos issuer
``` 

## **Lock CnGP Token and Datum to WAMT Validator**

Update sampleScriptParams with the issuer's key-hash
```sh
key-hash issuer 
``` 
Export WATM assets
```sh
jamb -w watm-validator
jamb -w watm-fmp
``` 


Generate script address
```sh
script-addr -p watm-validator
``` 


Save issuer's utoxs in environment variables U1 and U2
```sh
utxos issuer 
``` 
Build sign and sumbit transaction to send CnGPT to WATM-vaidator.

```sh
cardano-cli transaction build \
--tx-in $U1 \
--tx-in $U2 \
--tx-out $(addr watm-validator)+10000000+"1 $T" \
--tx-out-inline-datum-file $DATA_PATH/watmDatum.json \
--change-address $(addr issuer) \
--out-file $TX_PATH/cngpt-lock.raw

tx-sign cngpt-lock issuer

tx-submit cngpt-lock
``` 

utxos watm-validator

--------------

## **Mint using WAMT FM Policy and Validator**

A user should query the datum locked at watm-validator and update it accordingly. We cannot do that from cardano-cli but we have some datums pre-exported

generate keys for alice and bob
send each 100 ada from issuer

### **Mint 1 NFT for alice using "ADA" voucher**
Save a utxo from alice in UA variable
```sh
utxos alice
```

Save the utxo containting the CnGPT in UWATM variable
```sh
utxos watm-validator
```

Save the WATM NFT name
```sh
TN=$(token -p watm-fmp ADA)
```

Build sign and sumbit transaction to mint.
```sh
cardano-cli transaction build \
--tx-in $UA \
--tx-in $UWATM \
--tx-in-collateral $UA \
--tx-in-script-file $PLUTUS_SCRIPTS_PATH/watm-validator.plutus \
--tx-in-redeemer-file $DATA_PATH/ADARedeemer.json \
--tx-in-inline-datum-present \
--tx-out $(addr issuer)+10000000 \
--tx-out $(addr watm-validator)+10000000+"1 $T" \
--tx-out-inline-datum-file $DATA_PATH/watmDatum1.json \
--change-address $(addr alice) \
--mint "1 $TN" \
--mint-script-file $PLUTUS_SCRIPTS_PATH/watm-fmp.plutus \
--mint-redeemer-file  $DATA_PATH/unit.json \
--witness-override 2 \
--out-file $TX_PATH/alice-mint.raw

tx-sign alice-mint alice

tx-submit alice-mint
``` 
