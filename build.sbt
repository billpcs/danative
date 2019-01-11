enablePlugins(ScalaNativePlugin)
scalaVersion := "2.11.11"

// optimize for best runtime performance at expense of longer compilation time
nativeMode := "release"

// since this is a short-running command-line application, no GC is probably needed
nativeGC := "none"