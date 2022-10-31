
with Interfaces.C;
with Interfaces.C.Pointers;

with C_Types; use C_Types;

--  An Ada array corresponds to a C pointer to the first element.
package API_Vectors_Matrices is

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

   type API_Int_Array is array (Int range <>) of aliased Int;
   pragma Convention (C, API_Int_Array);

   type API_2D_Int_Array is array (Int range <>) of aliased API_Int_Array;
   pragma Convention (C, API_2D_Int_Array);

   package Int_Array_Pointers is new Interfaces.C.Pointers
     (Index => Int, Element => API_Int_Array, Element_Array => API_2D_Int_Array,
      Default_Terminator => API_Int_Array'(others => <>));

   type API_Unsigned_Array is array
     (Interfaces.C.unsigned range <>) of aliased Interfaces.C.unsigned;
   pragma Convention (C, API_Unsigned_Array);

   package Unsigned_Array_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, Interfaces.C.unsigned, API_Unsigned_Array,
      16#7FFFFFFF#);
   subtype Unsigned_Array_Pointer is Unsigned_Array_Pointers.Pointer;

   type API_Float is new Interfaces.C.Double;
   type API_Int is new Interfaces.C.Int;

   --     type API_Int_Array is array (API_Int range <>) of aliased API_Int;
   --
   --     package API_Int_Pointers is
   --       new Interfaces.C.Pointers
   --         (Index => API_Int,  Element => API_Int,
   --          Element_Array => API_Int_Array, Default_Terminator => 0);
   --     subtype API_Int_Ptr is API_Int_Pointers.Pointer;
   --
   --     type API_Int_Ptr_Array is array (API_Int range <>)
   --       of aliased API_Int_Ptr;
   --     package API_Int_Pointers_To_Pointers is
   --       new Interfaces.C.Pointers
   --         ( Index => API_Int, Element => API_Int_Ptr,
   --           Element_Array => API_Int_Ptr_Array, Default_Terminator => null);
   --     subtype API_Int_Ptr_Ptr is API_Int_Pointers_To_Pointers.Pointer;

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

end API_Vectors_Matrices;
