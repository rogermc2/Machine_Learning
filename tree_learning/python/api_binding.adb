
with Interfaces.C;

package body API_Binding is

   function API_Integer_2D (A, B : Integer_Matrix) return API_Pointers is
      First_A_Col : constant Integer := A'First (2);
      First_B_Col : constant Integer := B'First (2);
      A_Value     : aliased Integer;
      B_Value     : aliased Integer;
      Pointers    : API_Pointers (A'Length);
   begin
      for row in A'range loop
         A_Value := A (row, First_A_Col);
         Pointers.A_Ptrs (row) := A_Value'Unchecked_Access;
      end loop;

      for row in B'range loop
         B_Value := B (row, First_B_Col);
         Pointers.B_Ptrs (row) := B_Value'Unchecked_Access;
      end loop;

      return Pointers;

   end API_Integer_2D;

   --  -------------------------------------------------------------------------

   function API_4D (A : NL_Types.Boolean_List_2D;
                    B, C, D : ML_Types.Unbounded_List) return API_Pointers is
      use  Ada.Strings.Unbounded;
      --        First_A_Col : constant NL_Types.Boolean_List := A'First (2);
      A_Matrix    : constant Boolean_Matrix := To_Boolean_Matrix (A);
      A_Value     : aliased Interfaces.C.int;
      B_Array     : constant Unbounded_String_Array := To_Unbound_Array (B);
      C_Array     : constant Unbounded_String_Array := To_Unbound_Array (C);
      D_Array     : constant Unbounded_String_Array := To_Unbound_Array (D);
      B_Value     : aliased Unbounded_String;
      C_Value     : aliased Unbounded_String;
      D_Value     : aliased Unbounded_String;
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
         B_Value := B_Array (row);
         Pointers.B_Ptrs (row) := B_Value'Unchecked_Access;
         C_Value := C_Array (row);
         Pointers.C_Ptrs (row) := C_Value'Unchecked_Access;
         D_Value := D_Array (row);
         Pointers.D_Ptrs (row) := D_Value'Unchecked_Access;
      end loop;

      return Pointers;

   end API_4D;

   --  -------------------------------------------------------------------------

   function Get_A_Int_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array is
   begin
      return Ptrs.A_Ptrs;

   end Get_A_Int_Ptrs;

   --  -------------------------------------------------------------------------

   function Get_B_Int_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array is
   begin
      return Ptrs.B_Ptrs;

   end Get_B_Int_Ptrs;

   --  -------------------------------------------------------------------------

   function Get_A_Ptrs (Ptrs : API_4D_Pointers) return API_Int_Pointer_Array is
   begin
      return Ptrs.A_Ptrs;

   end Get_A_Ptrs;

   --  -------------------------------------------------------------------------

   function Get_B_Ptrs (Ptrs : API_4D_Pointers) return API_Int_Pointer_Array is
   begin
      return Ptrs.B_Ptrs;

   end Get_B_Ptrs;

   --  -------------------------------------------------------------------------

   function Get_C_Ptrs (Ptrs : API_4D_Pointers) return API_Int_Pointer_Array is
   begin
      return Ptrs.C_Ptrs;

   end Get_C_Ptrs;

   --  -------------------------------------------------------------------------

   function Get_D_Ptrs (Ptrs : API_4D_Pointers) return API_Int_Pointer_Array is
   begin
      return Ptrs.D_Ptrs;

   end Get_D_Ptrs;

   --  -------------------------------------------------------------------------

end API_Binding;
