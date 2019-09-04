---
library: fx
---

{: data-executable="true"}
```kotlin
import arrow.core.extensions.either.fx.fx

fun saveTheEarth(): Either<NukeException, Impacted> =
  Either.fx {
    val nuke = !arm()
    val meteor = !aim()
    val impact = !launch(meteor, nuke)
    impact
  }

saveTheEarth()
```