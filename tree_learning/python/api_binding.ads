
with Interfaces.C.Pointers;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package API_Binding is
   type API_Int_Pointer_Array (<>) is private;
   type API_Pointers (Ptrs_Length : Integer) is private;

   function API_2D (A, B : Integer_Matrix) return API_Pointers;
   function Get_A_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array;
   function Get_B_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array;

private
   type Integer_Array is array (Integer range <>) of aliased Integer;

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

end API_Binding;
