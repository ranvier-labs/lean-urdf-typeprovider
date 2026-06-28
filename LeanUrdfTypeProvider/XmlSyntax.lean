import Lean

/-!
# XML Syntax Extension

This module intentionally uses Lean's parser-extension surface (`syntax`,
`declare_syntax_cat`, and command elaboration consumers), not `Parsec`.

The grammar is the URDF-relevant XML subset:

* elements with string-valued attributes,
* empty element tags,
* nested element tags,
* optional XML declaration.

The elaborator later checks matching start/end tags and URDF-specific shape.
-/

namespace LeanUrdfTypeProvider.XmlSyntax

declare_syntax_cat urdfXmlAttr
declare_syntax_cat urdfXmlElement
declare_syntax_cat urdfXmlDocument

syntax ident "=" str : urdfXmlAttr

syntax "<" ident urdfXmlAttr* "/>" : urdfXmlElement
syntax "<" ident urdfXmlAttr* ">" urdfXmlElement* "</" ident ">" : urdfXmlElement

syntax urdfXmlElement : urdfXmlDocument
syntax "<?" "xml" urdfXmlAttr* "?>" urdfXmlElement : urdfXmlDocument

end LeanUrdfTypeProvider.XmlSyntax
