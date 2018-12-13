{-# LANGUAGE CPP #-}
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
  string,

  -- code generators
  def_struct,
  -- funargs_struct,
  ref_eip712StructRepLiteral,
  ref_eip712HashStruct,
  ref_struct_member,
  unpack_struct,
) where

#define WORD_SIZE 32

import RIO
-- import qualified RIO.Text as T

import Solidity.QQUtils

newtype Name = Name Text
  deriving (IsString, ToMarkup, Semigroup)

data Type
  = Word Name{-e.g. uint256-}
  | Array Name{-e.g. bytes-}

address, uint256, bytes32 :: Name -> (Name, Type)
address = (, Word "address")
uint256 = (, Word "uint256")
bytes32 = (, Word "bytes32")

string :: Name -> (Name, Type)
string = (, Array "string")

isWord :: Type -> Bool
isWord t = case t of
  (Word _) -> True
  _        -> False

instance ToMarkup Type where
  toMarkup ty = case ty of
    (Word nm)  -> toMarkup nm
    (Array nm) -> toMarkup nm

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
  // HLL CODEGEN: ${annot}
  struct ${name} {
    %{forall (membername, ty) <- membs}
      ${ty} ${membername};
    %{endforall}
  }|]

-- Enumerate in reverse index order
enumerateDesc :: [a] -> [(Int, a)]
enumerateDesc xs = reverse $ zip [0..] (reverse xs)

-- Is there a prelude function for this
enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..] xs

-- `encodeType`
-- Nested structs not supported yet
ref_eip712StructRepLiteral :: Struct -> Text
ref_eip712StructRepLiteral (Struct name membs _) = renderMarkupStrict
  -- sorry for the ugliness - need to control whitespace to adhere to spec.
  [sol|"${name}(%{forall (ix, (name, ty)) <- xs}${ty} ${name}${sep ix}%{endforall})"|]
    where
      xs = enumerateDesc membs
      sep ix = if ix > 0 then "," else ""::Text

-- Generate a call to keccak256 the abi packed encoding of a struct.
-- only works when members are all single word types.
ref_eip712HashStruct:: Name -> Struct -> Markup
ref_eip712HashStruct varName s@(Struct _ membs _) =
  [sol|keccak256(abi.encode(
        keccak256(bytes(${ref_eip712StructRepLiteral s})),
    %{forall (ix, (nm, ty)) <- xs}
      %{if isWord ty}
        ${varName}.${nm}${sep ix}
      %{else}
        keccak256(bytes(${varName}.${nm}))${sep ix}
      %{endif}
    %{endforall}
    ))
  |]
    where
      xs = enumerateDesc membs
      -- TODO abstract this function
      sep ix = if ix > 0 then "," else ""::Text

-- We can't pass structs via call data yet, so we pass them via bytes32 array
-- and unpack them into a struct.
unpack_struct :: Struct -> Name -> Name -> Markup
unpack_struct (Struct _ members _) srcVar dstVar = [sol|
  // HLL CODEGEN: unpack ${srcVar} into ${dstVar}
  assembly {
  %{forall (ix, (_nm, _ty)) <- xs}
    mstore(add(${dstVar}, ${ix}), calldataload(add(${srcVar}, ${ix})))
  %{endforall}
  }
  |]
  where
    xs = map (first (*32)) $ enumerate members

ref_struct_member :: Name -> Name -> Name
ref_struct_member varName memberName = "__" <> varName <> "_" <> memberName

{-funargs_struct :: Name -> Struct -> Markup
funargs_struct varName (Struct name membs _) =
  [sol|
  // HLL CODEGEN: ${name} ${varName}
  %{forall (ix, (membername, ty)) <- xs}
    ${ty} ${ref_struct_member varName membername}${sep ix}
  %{endforall}
   |]
   where
     xs = enumerateDesc membs
     sep ix = if ix > 0 then "," else ""::Text
     -}
