[<AutoOpen>]
[<System.ObsoleteAttribute>] //marking as obsolete is a workaround F# not honoring EditorBrowsable(EditorBrowsableState.Never) to hide intellisense discoverability, thanks to Tomas Petricek's answer on SO: http://stackoverflow.com/questions/6527141/is-it-possible-to-mark-a-module-function-as-hidden-from-intellisense-discovery/6527933#6527933
module Swensen.Unquote.Prelude

#nowarn "42" //for raises (inline IL)

///raise is not inlined in Core.Operators, so shows up in stack traces.  We inline it here for clean stacktraces.
let inline raise (e: System.Exception) = (# "throw" e : 'U #)

