# Mod10

Verify numeric strings.

```elm
import Mod10

Mod10.verify "79927398713" == True
Mod10.verify "79927398714" == False
Mod10.verify "nonsense" == False
```

Calculate a "check digit" for a numeric string.

```elm
import Mod10

Mod10.calculate "7992739871" == Just '3'
Mod10.calculate "nonsense" == Nothing
```
