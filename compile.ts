
// ($)  :: (a -> b) ->  a -> b
type Universal = any

function show(p: Universal): string {
  return JSON.stringify(p)
}


// Use: $(show, [1, 2, 3, 4]), 
// Output: "[1,2,3,4]" 
function $<T>(fn: T) {
  return fn
}

// Use: applicative(v => show(v), [1,2,3,4]) 
// Output: ["1", "2", "3", "4"]
function applicative<
  T extends Array<T>
>(fn: (a: Universal) => Universal, values: Universal) {
  if (values.length) return values.map(fn)

  return "todo: applicative (<$>) operator"
}

// Use: semiGroup("Short Text", " Long Text")
function semiGroup<T extends string>(s: T, l: T) {
  return s + l
}

function rightAssoc<
  T extends Array<T>,
>(val: T, fn: (val: string) => T) {
  const values = String(val).split('')
  
  if (val.length) return values.map(v => fn(v))

  return []
}

type FSequence<T> = (x: T, y: T) => T;
type Sequence<T> = (x: T) => T;

// Use: seq((x, y) => x + y, (_) => "Right", "Left")
// Output: "LeftRight"
function seq<T>(fa: FSequence<T>, fb: Sequence<T>, val: T) {
    return fa(val, fb(val))
}
