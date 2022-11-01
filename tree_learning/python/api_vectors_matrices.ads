
with Interfaces.C;
with Interfaces.C.Pointers;

with C_Types; use C_Types;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

--  An Ada array corresponds to a C pointer to the first element.
--  The type Pointer is C-compatible and corresponds to one use of a
--  C element.
--  An object of type Pointer is interpreted as a pointer to the initial
--  Element of an Element_Array.
--  For example, see
--  https://www.adaic.org/resources/add_content/standards/05rm/html/RM-B-3-2.html
package API_Vectors_Matrices is

    type Buffer_Target (<>) is tagged limited private;
    type Buffer_Usage is private;

    package Int_Pointers is new Interfaces.C.Pointers
      (Size, Int, Int_Array, Int'Last);

    --  It is erroneous to dereference a Pointer that does not designate an aliased Element.
    --      type API_Vector_2D is record
    --          X : aliased Interfaces.C.C_float;
    --          Y : aliased Interfaces.C.C_float;
    --      end record;
    --      pragma Convention (C_Pass_By_Copy, API_Vector_2D);
    --
    --      type API_Vector_2D_Array is array
    --        (Interfaces.C.unsigned range <>) of aliased API_Vector_2D;
    --      pragma Convention (C, API_Vector_2D_Array);
    --
    --      type API_Vector_3D is record
    --          X : aliased Interfaces.C.C_float;
    --          Y : aliased Interfaces.C.C_float;
    --          Z : aliased Interfaces.C.C_float;
    --      end record;
    --      pragma Convention (C_Pass_By_Copy, API_Vector_3D);
    --
    --      type API_Vector_3D_Array is array
    --        (Interfaces.C.unsigned range <>) of aliased API_Vector_3D;
    --      pragma Convention (C, API_Vector_3D_Array);

    type API_Unsigned_Array is array
      (Interfaces.C.unsigned range <>) of aliased Interfaces.C.unsigned;
    pragma Convention (C, API_Unsigned_Array);

    package Unsigned_Array_Pointers is new Interfaces.C.Pointers
      (Interfaces.C.unsigned, Interfaces.C.unsigned, API_Unsigned_Array,
       16#7FFFFFFF#);
    subtype Unsigned_Array_Pointer is Unsigned_Array_Pointers.Pointer;

    type API_Float is new Interfaces.C.Double;
    type API_Int is new Interfaces.C.Int;

    type API_Int_Array is array (API_Int range <>) of aliased API_Int;

    package API_Int_Pointers is
      new Interfaces.C.Pointers
        (Index => API_Int,  Element => API_Int,
         Element_Array => API_Int_Array, Default_Terminator => 0);
    subtype API_Int_Ptr is API_Int_Pointers.Pointer;

    type API_Int_Ptr_Array is array (API_Int range <>)
      of aliased API_Int_Ptr;
    package API_Int_Pointers_To_Pointers is
      new Interfaces.C.Pointers
        ( Index => API_Int, Element => API_Int_Ptr,
          Element_Array => API_Int_Ptr_Array, Default_Terminator => null);
    subtype API_Int_Ptr_Ptr is API_Int_Pointers_To_Pointers.Pointer;

    type API_Float_Array is array (API_Int range <>) of aliased API_Float;
    package API_Float_Pointers is
      new Interfaces.C.Pointers
        (Index => API_Int,  Element => API_Float,
         Element_Array => API_Float_Array, Default_Terminator => 0.0);
    subtype API_Float_Ptr is API_Float_Pointers.Pointer;

    type API_Float_Ptr_Array is array (API_Int range <>)
      of aliased API_Float_Ptr;
    package API_Float_Pointers_To_Pointers is
      new Interfaces.C.Pointers
        ( Index => API_Int, Element => API_Float_Ptr,
          Element_Array => API_Float_Ptr_Array, Default_Terminator => null);
    subtype API_Float_Ptr_Ptr is API_Float_Pointers_To_Pointers.Pointer;

    procedure Allocate (Target : Buffer_Target; Number_Of_Bytes : Long;
                        Usage  : Buffer_Usage);
    procedure API_2D (A, B : Integer_Matrix);

    generic
        with package Pointers is new Interfaces.C.Pointers (<>);
    procedure Load_To_Buffer (Target : Buffer_Target'Class;
                              Data   : Pointers.Element_Array;
                              Usage  : Buffer_Usage);
private
    --  This type is never used directly. However, enumerations refer to it for
    --  defining their Size attribute.
    subtype Enum is Interfaces.C.unsigned;
    --  These types are actually not pointers.
    subtype IntPtr is Interfaces.C.long;
    subtype SizeIPtr is Interfaces.C.long;

    type Buffer_Kind is (Array_Buffer, Element_Array_Buffer, Pixel_Pack_Buffer,
                         Pixel_Unpack_Buffer, Uniform_Buffer, Texture_Buffer,
                         Transform_Feedback_Buffer, Transform_Feedback,
                         Copy_Read_Buffer, Copy_Write_Buffer,
                         Draw_Indirect_Buffer, Shader_Storage_Buffer,
                         Atomic_Counter_Buffer);

    type Buffer_Target (Kind : Buffer_Kind) is tagged limited null record;

    type Buffer_Usage is (Stream_Draw, Stream_Read, Stream_Copy,
                          Static_Draw, Static_Read, Static_Copy,
                          Dynamic_Draw, Dynamic_Read, Dynamic_Copy);
    for Buffer_Usage use (Stream_Draw  => 16#88E0#,
                          Stream_Read  => 16#88E1#,
                          Stream_Copy  => 16#88E2#,
                          Static_Draw  => 16#88E4#,
                          Static_Read  => 16#88E5#,
                          Static_Copy  => 16#88E6#,
                          Dynamic_Draw => 16#88E8#,
                          Dynamic_Read => 16#88E9#,
                          Dynamic_Copy => 16#88EA#);
    for Buffer_Usage'Size use Enum'Size;

end API_Vectors_Matrices;
