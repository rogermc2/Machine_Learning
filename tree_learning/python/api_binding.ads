
with Interfaces.C.Pointers;

with Ada.Strings.Unbounded;

with ML_Types;
with NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package API_Binding is
   type API_Int_Pointer_Array (<>) is private;
   type API_Pointers (Ptrs_Length : Integer) is private;
   type API_4D_Pointers (Ptrs_Length : Integer) is private;

   function API_Integer_2D (A, B : Integer_Matrix) return API_Pointers;
   function API_4D (A : NL_Types.Boolean_List_2D;
                    B, C, D : ML_Types.Unbounded_List) return API_Pointers;
   function Get_A_Int_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array;
   function Get_B_Int_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array;
   function Get_A_Ptrs (Ptrs : API_4D_Pointers) return API_Int_Pointer_Array;
   function Get_B_Ptrs (Ptrs : API_4D_Pointers) return API_Int_Pointer_Array;
   function Get_C_Ptrs (Ptrs : API_4D_Pointers) return API_Int_Pointer_Array;
   function Get_D_Ptrs (Ptrs : API_4D_Pointers) return API_Int_Pointer_Array;

private
   type Boolean_Array is array (Integer range <>) of aliased Boolean;
   type Integer_Array is array (Integer range <>) of aliased Integer;
   type Unbounded_Array is array (Integer range <>)
     of aliased Ada.Strings.Unbounded.Unbounded_String;

   package API_Int_Pointers is new Interfaces.C.Pointers
     (Index => Integer, Element => Integer,
      Element_Array => Integer_Array,
      Default_Terminator => 0);

   type API_Int_Pointer_Array is array (Integer range <>)
     of API_Int_Pointers.Pointer;

   type API_Pointers (Ptrs_Length : Integer) is record
      A_Ptrs : API_Int_Pointer_Array (1 .. Ptrs_Length);
      B_Ptrs : API_Int_Pointer_Array (1 .. Ptrs_Length);
   end record;

   type API_Boolean_Pointer_Array is array (Integer range <>)
     of API_Boolean_Pointers.Pointer;
   type API_Unbound_Pointer_Array is array (Integer range <>)
     of API_Unbound_Pointers.Pointer;

   type API_4D_Pointers (Ptrs_Length : Integer) is record
      A_Ptrs : API_Boolean_Pointer_Array (1 .. Ptrs_Length);
      B_Ptrs : API_Unbound_Pointer_Array (1 .. Ptrs_Length);
      C_Ptrs : API_Unbound_Pointer_Array (1 .. Ptrs_Length);
      D_Ptrs : API_Unbound_Pointer_Array (1 .. Ptrs_Length);
   end record;

   type API_4D_Pointer_Array is array (Integer range <>)
     of API_4D_Pointers.Pointer;

end API_Binding;
