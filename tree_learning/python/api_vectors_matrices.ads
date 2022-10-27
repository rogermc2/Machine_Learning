
with Interfaces.C;
with Interfaces.C.Pointers;

package API_Vectors_Matrices is

   --  It is erroneous to dereference a Pointer that does not designate an aliased Element.
   type API_Vector_2D is record
      X : aliased Interfaces.C.C_float;
      Y : aliased Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Vector_2D);

   type API_Vector_3D is record
      X : aliased Interfaces.C.C_float;
      Y : aliased Interfaces.C.C_float;
      Z : aliased Interfaces.C.C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Vector_3D);

   type API_Vector_2D_Array is array
     (Interfaces.C.unsigned range <>) of aliased API_Vector_2D;
   pragma Convention (C, API_Vector_2D_Array);

   type API_Vector_3D_Array is array
     (Interfaces.C.unsigned range <>) of aliased API_Vector_3D;
   pragma Convention (C, API_Vector_3D_Array);

   type API_Unsigned_Array is array
     (Interfaces.C.unsigned range <>) of aliased Interfaces.C.unsigned;
   pragma Convention (C, API_Unsigned_Array);

   package Unsigned_Array_Pointers is new Interfaces.C.Pointers
     (Interfaces.C.unsigned, Interfaces.C.unsigned, API_Unsigned_Array,
      16#7FFFFFFF#);
   subtype Unsigned_Array_Pointer is Unsigned_Array_Pointers.Pointer;

end API_Vectors_Matrices;
