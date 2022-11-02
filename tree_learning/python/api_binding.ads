
with Interfaces.C.Pointers;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package API_Binding is

    type API_Int_Pointer_Array (<>) is private;

    procedure API_2D (A, B : aliased Integer_Matrix;
                      A_Ptrs, B_Ptrs : out API_Int_Pointer_Array);

private
    type Integer_Array is array (Integer range <>) of aliased Integer;

    package API_Int_Pointers is new Interfaces.C.Pointers
      (Index => Integer, Element => Integer,
       Element_Array => Integer_Array,
       Default_Terminator => 0);

    type API_Int_Pointer_Array is array (Integer range <>)
      of API_Int_Pointers.Pointer;

end API_Binding;
