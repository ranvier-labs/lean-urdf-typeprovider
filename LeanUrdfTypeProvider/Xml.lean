/-!
# XML at run time

A small reader for the XML that robot descriptions use: elements, attributes in single or
double quotes, empty-element tags, the XML declaration and other processing instructions,
comments, `<!DOCTYPE …>`, CDATA and character data (skipped), and the five predefined
entities plus numeric character references in attribute values.

Unlike the syntax-category grammar in `XmlSyntax`, it reads any file, including the
comments, hyphenated names and namespaced attributes (`xmlns:xacro`) of real URDFs.
-/

namespace LeanUrdfTypeProvider

structure XmlAttr where
  name : String
  value : String
  deriving Repr, Inhabited, BEq

structure XmlElement where
  name : String
  attrs : Array XmlAttr := #[]
  children : Array XmlElement := #[]
  deriving Repr, Inhabited

namespace XmlElement

def attr? (elem : XmlElement) (name : String) : Option String :=
  elem.attrs.findSome? fun attr => if attr.name == name then some attr.value else none

def requiredAttr (elem : XmlElement) (name : String) : Except String String :=
  match elem.attr? name with
  | some value => pure value
  | none => .error s!"<{elem.name}> is missing required attribute '{name}'"

def childrenNamed (elem : XmlElement) (name : String) : Array XmlElement :=
  elem.children.filter (·.name == name)

def child? (elem : XmlElement) (name : String) : Option XmlElement :=
  elem.children.find? (·.name == name)

end XmlElement

namespace Xml

private structure Reader where
  chars : Array Char
  pos : Nat := 0

private abbrev ReadM := StateT Reader (Except String)

private def peek : ReadM (Option Char) := do
  let r ← get
  return r.chars[r.pos]?

private def lineOf (r : Reader) : Nat :=
  (r.chars.extract 0 r.pos).foldl (fun n c => if c == '\n' then n+1 else n) 1

private def fail (msg : String) : ReadM α := do
  throw s!"XML line {lineOf (← get)}: {msg}"

private def advance (n : Nat := 1) : ReadM Unit :=
  modify fun r => {r with pos := r.pos+n}

private def startsWith (s : String) : ReadM Bool := do
  let r ← get
  let cs := s.toList
  return (cs.zipIdx.all fun (c, i) => r.chars[r.pos+i]? == some c)

private def expect (s : String) : ReadM Unit := do
  if ← startsWith s then advance s.length else fail s!"expected '{s}'"

private partial def skipSpace : ReadM Unit := do
  match ← peek with
  | some c => if c.isWhitespace then advance; skipSpace
  | none => pure ()

/-- Skip past the next occurrence of `stop`. -/
private partial def skipPast (stop : String) : ReadM Unit := do
  if ← startsWith stop then advance stop.length
  else match ← peek with
    | some _ => advance; skipPast stop
    | none => fail s!"unterminated construct, expected '{stop}'"

private def isNameChar (c : Char) : Bool :=
  c.isAlphanum || c == '_' || c == '-' || c == '.' || c == ':'

private partial def readName : ReadM String := do
  let rec go (acc : String) : ReadM String := do
    match ← peek with
    | some c => if isNameChar c then advance; go (acc.push c) else pure acc
    | none => pure acc
  let name ← go ""
  if name.isEmpty then fail "expected a name" else pure name

private def decodeEntity (e : String) : Option Char :=
  match e with
  | "lt" => some '<' | "gt" => some '>' | "amp" => some '&' | "quot" => some '"' | "apos" => some '\''
  | _ =>
    if e.startsWith "#x" then (String.toNat? ("0x" ++ (e.drop 2).toString)).map Char.ofNat
      |>.orElse fun _ => hexValue (e.drop 2).toString
    else if e.startsWith "#" then ((e.drop 1).toString.toNat?).map Char.ofNat
    else none
where
  hexValue (s : String) : Option Char :=
    s.foldl (fun acc c => acc.bind fun n =>
      if c.isDigit then some (n*16 + (c.toNat - '0'.toNat))
      else if 'a' ≤ c && c ≤ 'f' then some (n*16 + (c.toNat - 'a'.toNat + 10))
      else if 'A' ≤ c && c ≤ 'F' then some (n*16 + (c.toNat - 'A'.toNat + 10))
      else none) (some 0) |>.map Char.ofNat

private partial def readValue : ReadM String := do
  let quote ← match ← peek with
    | some '"' => pure '"'
    | some '\'' => pure '\''
    | _ => fail "expected a quoted attribute value"
  advance
  let rec go (acc : String) : ReadM String := do
    match ← peek with
    | none => fail "unterminated attribute value"
    | some c =>
      if c == quote then advance; pure acc
      else if c == '&' then
        advance
        let rec entity (e : String) : ReadM String := do
          match ← peek with
          | some ';' => advance; pure e
          | some c => if e.length > 10 then fail "bad entity" else advance; entity (e.push c)
          | none => fail "unterminated entity"
        let e ← entity ""
        match decodeEntity e with
        | some ch => go (acc.push ch)
        | none => fail s!"unknown entity '&{e};'"
      else advance; go (acc.push c)
  go ""

/-- Skip comments, processing instructions, DOCTYPE, CDATA and character data. -/
private partial def skipMisc : ReadM Unit := do
  skipSpace
  if ← startsWith "<!--" then skipPast "-->"; skipMisc
  else if ← startsWith "<?" then skipPast "?>"; skipMisc
  else if ← startsWith "<![CDATA[" then skipPast "]]>"; skipMisc
  else if ← startsWith "<!" then skipPast ">"; skipMisc
  else match ← peek with
    | some '<' => pure ()
    | some _ => advance; skipMisc
    | none => pure ()

private partial def readElement : ReadM XmlElement := do
  expect "<"
  let name ← readName
  let rec attrs (acc : Array XmlAttr) : ReadM (Array XmlAttr × Bool) := do
    skipSpace
    if ← startsWith "/>" then advance 2; pure (acc, true)
    else if ← startsWith ">" then advance; pure (acc, false)
    else
      let n ← readName
      skipSpace; expect "="; skipSpace
      let v ← readValue
      attrs (acc.push ⟨n, v⟩)
  let (as, empty) ← attrs #[]
  if empty then return {name, attrs := as}
  let rec body (acc : Array XmlElement) : ReadM (Array XmlElement) := do
    skipMisc
    if ← startsWith "</" then
      advance 2
      let close ← readName
      if close != name then fail s!"<{name}> closed by </{close}>"
      skipSpace; expect ">"
      pure acc
    else if (← peek).isNone then fail s!"<{name}> is not closed"
    else body (acc.push (← readElement))
  return {name, attrs := as, children := ← body #[]}

/-- The document element of an XML text. -/
def parse (source : String) : Except String XmlElement :=
  (do skipMisc; let e ← readElement; skipMisc
      if (← peek).isSome then fail "content after the document element"
      pure e).run' {chars := source.toList.toArray}

end Xml
end LeanUrdfTypeProvider
