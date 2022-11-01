
with System;

with Ada.Text_IO; use Ada.Text_IO;

package body API_Vectors_Matrices is

    procedure Buffer_Data
      (Target : Buffer_Kind;
       Size : SizeIPtr; Data : System.Address;
       Usage : Buffer_Usage);  --   with
    --        Dynamic => "glBufferData",
    --        Wrapper => "GL.Objects.Buffers.Load_To_Buffer",
    --        Wrapper => "GL.Objects.Buffers.Allocate";

    --  ------------------------------------------------------------------------

    --  Use this instead of Load_To_Buffer when you don't want to copy any data
    procedure Allocate (Target : Buffer_Target; Number_Of_Bytes : Long;
                        Usage  : Buffer_Usage) is
    begin
        Buffer_Data (Target.Kind, SizeIPtr (Number_Of_Bytes),
                     System.Null_Address, Usage);
    exception
        when others => Put_Line ("API_Vectors_Matrices.Allocate error");

    end Allocate;

    --  ------------------------------------------------------------------------

    procedure Buffer_Data
      (Target : Buffer_Kind;
       Size : SizeIPtr; Data : System.Address;
       Usage : Buffer_Usage) is  --  with
        --        Dynamic => "glBufferData",
        --        Wrapper => "GL.Objects.Buffers.Load_To_Buffer",
        --        Wrapper => "Allocate") is
    begin
        null;

    exception
        when others => Put_Line ("API_Vectors_Matrices.Buffer_Data error");

    end Buffer_Data;

    --  ------------------------------------------------------------------------

    procedure Load_To_Buffer (Target : Buffer_Target'Class;
                              Data   : Pointers.Element_Array;
                              Usage  : Buffer_Usage) is
        use type Interfaces.C.long;
    begin
        Buffer_Data (Target.Kind,
                     Pointers.Element'Size * Data'Length / System.Storage_Unit,
                     Data (Data'First)'Address, Usage);
    exception
        when others => Put_Line ("API_Vectors_Matrices.Load_To_Buffer error");

    end Load_To_Buffer;

    --  ------------------------------------------------------------------------

    procedure API_2D (A, B : Integer_Matrix) is

        type API_Int_Array is array (A'Range) of aliased Int;
        pragma Convention (C, API_Int_Array);

        type API_2D_Int_Array is array (Size range <>) of aliased API_Int_Array;
        pragma Convention (C, API_2D_Int_Array);

        package Int_Array_Pointers is new Interfaces.C.Pointers
          (Index => Size, Element => API_Int_Array, Element_Array => API_2D_Int_Array,
           Default_Terminator => API_Int_Array'(others => <>));

    begin
        null;

    exception
        when others => Put_Line ("API_Vectors_Matrices.API_2D error");

    end API_2D;

    --  ------------------------------------------------------------------------

end API_Vectors_Matrices;
