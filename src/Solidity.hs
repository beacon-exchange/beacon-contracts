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
  def_eip712StructTypeHash,
  ref_eip712StructTypeHash,
  ref_eip712StructRepLiteral,
  ref_eip712HashStruct,
  ref_struct_member,
) where

#define WORD_SIZE 32

import RIO
import qualified RIO.Text as T

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

{-
-- Is there a prelude function for this
enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0..] xs
-}

-- `encodeType`
-- Nested structs not supported yet
ref_eip712StructRepLiteral :: Struct -> Text
ref_eip712StructRepLiteral (Struct name membs _) = renderMarkupStrict
  -- sorry for the ugliness - need to control whitespace to adhere to spec.
  [sol|"${name}(%{forall (ix, (name, ty)) <- xs}${ty} ${name}${sep ix}%{endforall})"|]
    where
      xs = enumerateDesc membs
      sep ix = if ix > 0 then "," else ""::Text

-- collisions are unlikely but possible due to case insensitivity.
-- don't create struct names which only differ in case!
ref_eip712StructTypeHash :: Struct -> Text
ref_eip712StructTypeHash (Struct (Name name) _ _) =
  "__" <> upper_name <> "_EIP712_TYPEHASH"
  where
    upper_name = T.toUpper name

def_eip712StructTypeHash :: Struct -> Markup
def_eip712StructTypeHash s =
  -- double check me
  [sol|bytes32 constant ${ref_eip712StructTypeHash s} =
    keccak256(${ref_eip712StructRepLiteral s});|]

-- Generate a call to keccak256 the abi packed encoding of a struct.
-- only works when members are all single word types.
ref_eip712HashStruct:: Name -> Struct -> Markup
ref_eip712HashStruct varName s@(Struct _ membs _) =
  [sol|keccak256(abi.encode(
        ${ref_eip712StructTypeHash s},
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

-- In places where we can't create a struct, just unpack the values.
ref_struct_member :: Name -> Name -> Name
ref_struct_member varName memberName = "__" <> varName <> "_" <> memberName
