
with Interfaces.C;
with Interfaces.C.Strings;

--  with Ada.Strings.Unbounded;

package body API_Binding is

   type String_Access is access String;
   type String_Access_Array is array (Positive range <>) of String_Access;

   --  -------------------------------------------------------------------------

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
                    B, C, D : ML_Types.Bounded_String_List)
                    return API_4D_Pointers is
      use Interfaces.C;
      use  Interfaces.C.Strings;
      use  Ada.Strings.Unbounded;
      --        First_A_Col : constant NL_Types.Boolean_List := A'First (2);
      A_Matrix    : constant Boolean_Matrix := To_Boolean_Matrix (A);
      A_Values    : aliased array
        (1 .. A_Matrix'Length * A_Matrix'Length (2)) of aliased int;
      AV_Index    : Natural;
      B_Ptrs      : String_Access_Array (B.First_Index .. B.Last_Index);
      C_Ptrs      : String_Access_Array (C.First_Index .. C.Last_Index);
      D_Ptrs      : String_Access_Array (D.First_Index .. D.Last_Index);
      Pointers    : API_4D_Pointers (Integer (A.Length));
   begin
      for row in A_Matrix'Range loop
         for col in A_Matrix'Range (2) loop
            AV_Index := (row - 1) * A_Matrix'Length + col;
            if A_Matrix (row, col) then
               A_Values (AV_Index) := 1;
            else
               A_Values (AV_Index) := 0;
            end if;
         end loop;
         AV_Index := row * A_Matrix'Length;
         Pointers.A_Ptrs (row) := A_Values (AV_Index)'Unchecked_Access;
      end loop;

      for row in B_Ptrs'Range loop
            B_Ptrs (row) := New_String (B (row));
            C_Ptrs (row) := New_String (C (row));
            D_Ptrs (row) := New_String (D (row));
      end loop;

      Pointers.B_Ptrs := B_Ptrs;

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
