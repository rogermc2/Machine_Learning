
with System;

with Ada.Finalization;

package Py is

   --  Handle -- To Python object. Maintains a reference count automatically
   --            increased on assignment and decreased on finalization
   type Handle is new Ada.Finalization.Controlled with private;

   function No_Value return Handle;
   procedure Check_Handle (Object : Handle);
   procedure Invalidate (Object : in out Handle);
   function Is_Valid (Object : Handle) return Boolean;

   --  Encapsulated objects.  An Ada object passed  to Python as  an  opaque
   --  Python object. When passed back to Ada the Ada object can be accessed
   --  via the Python object.

   generic
      type Object_Type (<>) is private;
   package Generic_Capsule is
      type Object_Type_Ptr is access all Object_Type;

      --  Create -- A Python object encapsulating Ada object
      --    Value - To be stored in the Python object
      --  Returns a handle to the newly created Python object
      function Create (Value : Object_Type) return Handle;

      --  Get -- Ada contents of the Python object
      --    Value - A handle to
      --  Returns a pointer to the Ada object held in
      function Get (Value : Handle) return Object_Type_Ptr;

      --  Is_Valid -- Ada contents of the Python object
      --  Returns True if the handle points to a capsule
      function Is_Valid (Value : Handle) return Boolean;
   private
      type Capsule_Name is tagged null record;
   end Generic_Capsule;

private

   type Object is new System.Address;
   Null_Object : constant Object := Object (System.Null_Address);

   type Object_Ptr is access all Object;
   pragma Convention (C, Object_Ptr);

   type Handle is new Ada.Finalization.Controlled with record
      Ptr : aliased Object := Null_Object;
   end record;

end Py;
