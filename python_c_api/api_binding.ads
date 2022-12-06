
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with Ada.Strings.Unbounded;

with ML_Types;
with NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package API_Binding is
   type API_Boolean_Pointer_Array (<>) is private;
   type API_Int_Pointer_Array (<>)  is private;
   type API_Pointers (Ptrs_Length : Integer)  is private;
   type API_2D_Pointers (A_Length, B_Length : Integer) is private;
   type API_4D_Pointers (A_Length, B_Length,
                         C_Length, D_Length : Integer) is private;
   type API_Unbound_Pointer_Array (<>) is private;
   type Char_Ptr_Array (<>) is private;

   function API_Integer_2D (A, B : Integer_Matrix) return API_Pointers;
   function API_2D (A : NL_Types.Boolean_List_2D;
                    B : ML_Types.Bounded_String_List)
                    return API_2D_Pointers;
   function API_4D (A       : NL_Types.Boolean_List_2D;
                    B, C, D : ML_Types.Bounded_String_List)
                    return API_4D_Pointers;
   function Get_A_Int_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array;
   function Get_B_Int_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array;
   function Get_A_Ptrs (Ptrs : API_2D_Pointers)
                        return API_Boolean_Pointer_Array;
   function Get_B_Ptrs (Ptrs : API_2D_Pointers) return Char_Ptr_Array;
   function Get_A_Ptrs (Ptrs : API_4D_Pointers)
                        return API_Boolean_Pointer_Array;
   function Get_B_Ptrs (Ptrs : API_4D_Pointers) return Char_Ptr_Array;
   function Get_C_Ptrs (Ptrs : API_4D_Pointers) return Char_Ptr_Array;
   function Get_D_Ptrs (Ptrs : API_4D_Pointers) return Char_Ptr_Array;

private
   type Char_Ptr_Array is array (Integer range <>) of
     Interfaces.C.Strings.chars_ptr;

   type API_Boolean_Array is array (Natural range <>)
     of aliased Interfaces.C.int;
   pragma Convention (C, API_Boolean_Array);
   type API_Int_Array is array (Natural range <>)
     of aliased Interfaces.C.int;
   pragma Convention (C, API_Int_Array);

   type API_Unbounded_Array is array (Natural range <>)
     of aliased Ada.Strings.Unbounded.Unbounded_String;
   pragma Convention (C, API_Unbounded_Array);

   package API_Int_Pointers is new Interfaces.C.Pointers
     (Index => Natural, Element => Interfaces.C.int,
      Element_Array => API_Int_Array, Default_Terminator => 0);

   type API_Int_Pointer_Array is array (Integer range <>)
     of API_Int_Pointers.Pointer;
   pragma Convention (C, API_Int_Pointer_Array);

   type API_Pointers (Ptrs_Length : Integer) is record
      A_Ptrs : API_Int_Pointer_Array (1 .. Ptrs_Length);
      B_Ptrs : API_Int_Pointer_Array (1 .. Ptrs_Length);
   end record;

   package API_Unbound_Pointers is new Interfaces.C.Pointers
     (Index => Natural, Element => Interfaces.C.char,
      Element_Array => API_Unbounded_Array,
      Default_Terminator => Interfaces.C.nul);

   type API_Boolean_Pointer_Array is array (Integer range <>)
     of API_Int_Pointers.Pointer;
   pragma Convention (C, API_Boolean_Pointer_Array);

   type API_Unbound_Pointer_Array is array (Integer range <>)
     of API_Unbound_Pointers.Pointer;
   pragma Convention (C, API_Unbound_Pointer_Array);

   type API_2D_Pointers (A_Length, B_Length : Integer) is record
      A_Ptrs : API_Boolean_Pointer_Array (1 .. A_Length);
      B_Ptrs : Char_Ptr_Array (1 .. B_Length);
   end record;

   type API_4D_Pointers
     (A_Length, B_Length, C_Length, D_Length : Integer) is record
      A_Ptrs : API_Boolean_Pointer_Array (1 .. A_Length);
      B_Ptrs : Char_Ptr_Array (1 .. B_Length);
      C_Ptrs : Char_Ptr_Array (1 .. C_Length);
      D_Ptrs : Char_Ptr_Array (1 .. D_Length);
   end record;

end API_Binding;
