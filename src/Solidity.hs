module Solidity (
  module Solidity.QQUtils,
  Name(..),
  Type,
  Struct(..),

  -- smart constructors
  struct,
  uint256,
  bytes32,
  address,

  -- code generators
  def_struct,
  funargs_struct,
  ref_eip712StructRepLiteral,
  ref_hashstruct,
  ref_struct_member,
) where

import RIO
-- import qualified RIO.Text as T

import Solidity.QQUtils

newtype Name = Name Text
  deriving (IsString, ToMarkup, Semigroup)

data Type
  = Word Name{-e.g. uint256-}

address, uint256, bytes32 :: Name -> (Name, Type)
address = (, Word "address")
uint256 = (, Word "uint256")
bytes32 = (, Word "bytes32")
instance ToMarkup Type where
  toMarkup (Word nm) = toMarkup nm

data Struct = Struct
  { _name :: Name
  , _members :: [ (Name, Type) ]
  , _annotation :: Text
  }

struct :: Name -> [ (Name, Type) ] -> Struct
struct n ms = Struct n ms ""

{- note case statement is:
  %{case mx}
  %{of Just x}
  /* ${x} */
  %{of Nothing}
  /* nothing */
  %{endcase}
-}

def_struct :: Struct -> Markup
def_struct (Struct name membs annot) = [sol|
  /* HLL CODEGEN: ${annot} */
  struct ${name} {
    %{forall (membername, ty) <- membs}
      ${ty} ${membername};
    %{endforall}
  }|]

-- Enumerate in reverse index order
enumerateDesc :: [a] -> [(Int, a)]
enumerateDesc xs = reverse $ zip [0..] (reverse xs)

-- TODO double check this
ref_eip712StructRepLiteral :: Struct -> Text
ref_eip712StructRepLiteral (Struct name membs _) = renderMarkupStrict
  -- sorry for the ugliness - need to control whitespace to adhere to spec.
  [sol|"struct ${name}(%{forall (ix, (name, ty)) <- xs}${ty} ${name}${sep ix}%{endforall})"|]
    where
      xs = enumerateDesc membs
      sep ix = if ix > 0 then ", " else ""::Text

-- Generate a call to keccak256 the abi packed encoding of a struct.
ref_hashstruct :: Name -> Struct -> Markup
ref_hashstruct varName s@(Struct _ membs _) =
  [sol|keccak256(abi.encodePacked(${ref_eip712StructRepLiteral s},
    %{forall (ix, (nm, _ty)) <- xs}
      ${ref_struct_member varName nm}${sep ix}

    %{endforall}
    ));
  |]
    where
      xs = enumerateDesc membs
      -- TODO abstract this function
      sep ix = if ix > 0 then "," else ""::Text

ref_struct_member :: Name -> Name -> Name
ref_struct_member varName memberName = "__" <> varName <> "_" <> memberName

funargs_struct :: Name -> Struct -> Markup
funargs_struct varName (Struct name membs _) =
  [sol|
  /* HLL CODEGEN: ${name} ${varName} */
  %{forall (ix, (membername, ty)) <- xs}
    ${ty} ${ref_struct_member varName membername}${sep ix}

  %{endforall}
   |]
   where
     xs = enumerateDesc membs
     sep ix = if ix > 0 then "," else ""::Text
