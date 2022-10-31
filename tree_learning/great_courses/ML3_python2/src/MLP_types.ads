
with Interfaces.C;
with Interfaces.C.Pointers;

--  with GL.Algebra;

package MLP_Types is
    pragma Preelaborate;
    use Interfaces;

    --  signed integer types
    type Byte  is new C.signed_char;
    type Short is new C.short;
    type Int   is new C.int;
    type Long  is new C.long;

    subtype Size is Int range 0 .. Int'Last;
    subtype Long_Size is Long range 0 .. Long'Last;

    --  unsigned integer types
    type UByte  is new C.unsigned_char;
    type UShort is new C.unsigned_short;
    type UInt   is new C.unsigned;

    --  floating point types ("Single" is used to avoid conflicts with Float)
    type Single is new C.C_float;
    type Double is new C.double;

    --  array types
    type UShort_Array is array (Size range <>) of aliased UShort;
    type Int_Array    is array (Size range <>) of aliased Int;
    type UInt_Array   is array (Size range <>) of aliased UInt;
    type Single_Array is array (Size range <>) of aliased Single;
    type Double_Array is array (Size range <>) of aliased Double;

    pragma Convention (C, UShort_Array);
    pragma Convention (C, Int_Array);
    pragma Convention (C, UInt_Array);
    pragma Convention (C, Single_Array);
    pragma Convention (C, Double_Array);

--      type Compare_Function is (Never, Less, Equal, LEqual, Greater, Not_Equal,
--                                GEqual, Always);

    package UShort_Pointers is new Interfaces.C.Pointers
      (Size, UShort, UShort_Array, UShort'Last);

    package Int_Pointers is new Interfaces.C.Pointers
      (Size, Int, Int_Array, Int'Last);

    package UInt_Pointers is new Interfaces.C.Pointers
      (Size, UInt, UInt_Array, UInt'Last);

    package Single_Pointers is new Interfaces.C.Pointers
      (Size, Single, Single_Array, 0.0);

private

    --     for Compare_Function use (Never     => 16#0200#,
    --                               Less      => 16#0201#,
    --                               Equal     => 16#0202#,
    --                               LEqual    => 16#0203#,
    --                               Greater   => 16#0204#,
    --                               Not_Equal => 16#0205#,
    --                               GEqual    => 16#0206#,
    --                               Always    => 16#0207#);
    --     for Compare_Function'Size use UInt'Size;

end MLP_Types;
