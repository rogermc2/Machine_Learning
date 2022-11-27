
with Interfaces.C;

--  with Ada.Strings.Unbounded;

package body API_Binding is

   function API_Integer_2D (A, B : Integer_Matrix) return API_Pointers is
      use Interfaces.C;
      --        type Integer_Ptr is access Integer;
      First_A_Col : constant Integer := A'First (2);
      First_B_Col : constant Integer := B'First (2);
      A_Value     : aliased int;
      B_Value     : aliased int;
      Pointers    : API_Pointers (A'Length);  -- record of A and B pointer lists
   begin
      for row in A'range loop
         A_Value := int (A (row, First_A_Col));
         Pointers.A_Ptrs (row) := A_Value'Unchecked_Access;
      end loop;

      for row in B'range loop
         B_Value := int (B (row, First_B_Col));
         Pointers.B_Ptrs (row) := B_Value'Unchecked_Access;
      end loop;

      return Pointers;

   end API_Integer_2D;

   --  -------------------------------------------------------------------------

   function API_4D (A       : NL_Types.Boolean_List_2D;
                    B, C, D : ML_Types.Unbounded_List) return API_4D_Pointers is
      use Interfaces.C;
      use  Ada.Strings.Unbounded;
      --        First_A_Col : constant NL_Types.Boolean_List := A'First (2);
      A_Matrix    : constant Boolean_Matrix := To_Boolean_Matrix (A);
      A_Value     : aliased int;
      B_Array     : constant Unbounded_String_Array := To_Unbound_Array (B);
      C_Array     : constant Unbounded_String_Array := To_Unbound_Array (C);
      D_Array     : constant Unbounded_String_Array := To_Unbound_Array (D);
      Pointers    : API_4D_Pointers (Integer (A.Length));
   begin
      for row in A_Matrix'Range loop
         if A_Matrix (row, 1) then
            A_Value := 1;
         else
            A_Value := 0;
         end if;
         Pointers.A_Ptrs (row) := A_Value'Unchecked_Access;
      end loop;

      for row in B_Array'Range loop
         declare
            B_Value     : aliased char_array :=
                            To_C (To_String (B_Array (row)));
            C_Value     : aliased char_array :=
                            To_C (To_String (C_Array (row)));
            D_Value     : aliased char_array :=
                            To_C (To_String (D_Array (row)));
         begin
            Pointers.B_Ptrs (row) := B_Value'Unchecked_Access;
            Pointers.C_Ptrs (row) := C_Value'Unchecked_Access;
            Pointers.D_Ptrs (row) := D_Value'Unchecked_Access;
         end;
      end loop;

      return Pointers;

   end API_4D;

   --  ------------------------------------------------------------------------------------------------------------------------------

   function Get_A_Int_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array is
   begin
      return Ptrs.A_Ptrs;

   end Get_A_Int_Ptrs;

   --  ------------------------------------------------------------------------------------------------------------------------------
   function Get_B_Int_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array is
   begin
      return Ptrs.B_Ptrs;

   end Get_B_Int_Ptrs;

   --  ------------------------------------------------------------------------------------------------------------------------------

   function Get_A_Ptrs (Ptrs : API_4D_Pointers) return API_Boolean_Pointer_Array is
   begin
      return Ptrs.A_Ptrs;

   end Get_A_Ptrs;

   --  ------------------------------------------------------------------------------------------------------------------------------

   function Get_B_Ptrs (Ptrs : API_4D_Pointers) return API_Unbound_Pointer_Array is
   begin
      return Ptrs.B_Ptrs;

   end Get_B_Ptrs;

   --  ------------------------------------------------------------------------------------------------------------------------------

   function Get_C_Ptrs (Ptrs : API_4D_Pointers) return API_Unbound_Pointer_Array is
   begin
      return Ptrs.C_Ptrs;

   end Get_C_Ptrs;

   --  ------------------------------------------------------------------------------------------------------------------------------

   function Get_D_Ptrs (Ptrs : API_4D_Pointers) return API_Unbound_Pointer_Array is
   begin
      return Ptrs.D_Ptrs;

   end Get_D_Ptrs;

   --  ------------------------------------------------------------------------------------------------------------------------------

end API_Binding;
