
with Interfaces.C;

package body API_Binding is

   function API_2D (A, B : Integer_Matrix) return API_Pointers is
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

   end API_2D;

   --  -------------------------------------------------------------------------

   function Get_A_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array is
   begin
      return Ptrs.A_Ptrs;

   end Get_A_Ptrs;

   --  -------------------------------------------------------------------------

   function Get_B_Ptrs (Ptrs : API_Pointers) return API_Int_Pointer_Array is
   begin
      return Ptrs.B_Ptrs;

   end Get_B_Ptrs;

   --  -------------------------------------------------------------------------

end API_Binding;
