# Notes

## Vesting

### Get pubkeyhash of wallet

```
import Ledger
import Wallet.Emulator
:i Wallet
:t walletPubKey
:t pubKeyHash
pubKeyHash $ walletPubKey $ Wallet 1
```

### Get a deadline

```
import Ledger.TimeSlot
:t slotToBeginPOSIXTime
:i SlotConfig
import Data.Default
:i Default
:i SlotConfig
def :: SlotConfig
slotToBeginPOSIXTime def 10
slotToBeginPOSIXTime def 20
```