// ALL IS PURE FUNCTION: NO SIDE EFFECTS!
// THIS IS JUST IMPLEMENT HOW HASKELL {ABSTRACT} OPERATOR WORKS!
// NOT FULLY SAME, BUT THE WAY IT WORKS IS SAME

// ($)  :: (a -> b) ->  a -> b
type Universal = any

function show(p: Universal): string {
  return JSON.stringify(p)
}

// | From: $
// Use: $(show, [1, 2, 3, 4]), 
// Output: "[1,2,3,4]" 
function $<T>(fn: T) {
  return fn
}

// | From: <$>
// Use: applicative(v => show(v), [1,2,3,4]) 
// Output: ["1", "2", "3", "4"]
function applicative<
  T extends Array<T>
>(fn: (a: Universal) => Universal, values: Universal) {
  if (values.length) return values.map(fn)

  return "todo: applicative (<$>) operator"
}

// | From: <>
// Use: semiGroup("Short Text", " Long Text")
function semiGroup<T extends string>(s: T, l: T) {
  return s + l
}

// | From: >>=
// Use: rightAssoc("Hello", show)
// Output: ""H""e""l""l""o""
function rightAssoc<
  T extends Array<T> | string,
>(val: T, fn: (val: string) => T) {
  const values = String(val).split('')
  
  if (val.length) return values.map(v => fn(v)).join("")

  return []
}

// | From: <>
type FSequence<T> = (x: T, y: T) => T;
type Sequence<T> = (x: T) => T;

// | From: <*>
// Use: seq((x, y) => x + y, (_) => "Right", "Left")
// Output: "LeftRight"
function seq<T>(fa: FSequence<T>, fb: Sequence<T>, val: T) {
    return fa(val, fb(val))
}
