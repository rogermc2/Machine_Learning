
--  with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

with To_BMP;

package body Support_5A is

   function Get_Pixels
     (Image                     : ML_U8_Types.Unsigned_8_Array_3D;
      First_Row, Last_Row       : Positive;
      First_Column, Last_Column : Positive; D3 : Positive := 3)
      return ML_U8_Types.Unsigned_8_Array_3D is
      Routine_Name : constant String := "Support_5A.Get_Pixels ";
      Part         : ML_U8_Types.Unsigned_8_Array_3D
        (1 .. Last_Row - First_Row + 1, 1 .. Last_Column - First_Column + 1,
         1 .. D3);
      Part_Row     : Natural := 0;
      Part_Col     : Natural;
   begin
      Assert (Last_Row <= Image'Last, Routine_Name & "invalid Last_Row.");
      Assert (Last_Column <= Image'Last (2), Routine_Name & "invalid Last_Column.");
      for row in First_Row .. Last_Row loop
         Part_Row := Part_Row + 1;
         Part_Col := 0;
         for col in First_Column .. Last_Column loop
            Part_Col := Part_Col + 1;
            for pix in Image'Range (3) loop
               Part (Part_Row, Part_Col, pix) := Image (row, col, pix);
            end loop;
            if D3 = 4 then
               Part (Part_Row, Part_Col, 4) := 1;
            end if;
         end loop;
      end loop;

      return Part;

   end Get_Pixels;

   --  -------------------------------------------------------------------------

   function Get_Picture (File_Name : String)
                         return ML_U8_Types.Unsigned_8_Array_3D is
      use Ada.Characters.Handling;
      use ML_U8_Types;
      Routine_Name    : constant String := "Support_5A.Get_Picture ";
      File_Name_Upper : constant String := To_Upper (File_Name);
      File_Kind       : constant String :=
                          File_Name_Upper
                            (File_Name_Upper'Last - 3 .. File_Name_Upper'Last);
--        Swap            : Interfaces.Unsigned_8;
   begin
      if File_Kind = ".PNG" then
         declare
            Initial : constant Unsigned_8_Array_3D :=
                        Unsigned_8_Array_3D (To_BMP.Process (File_Name));
            Clipped : Unsigned_8_Array_3D
              (1 .. Initial'Length - 15, 1 .. Initial'Length (2) - 1,
               Initial'Range (3));
         begin
            for row in Clipped'Range loop
               for col in Clipped'Range (2) loop
                  for pix in Clipped'Range (3) loop
                     Clipped (row, col, pix) := Initial (row, col, pix);
                  end loop;
--                    Swap := Clipped (row, col, 2);
--                    Clipped (row, col, 2) := Clipped (row, col, 3);
--                    Clipped (row, col, 3) := Swap;
               end loop;
            end loop;

            return Clipped;
         end;

      elsif File_Kind = ".JPG" then
         return Unsigned_8_Array_3D (To_BMP.Process (File_Name));

      else
         Put_Line (Routine_Name & "unsupported image format " & File_Kind);
         declare
            Dummy : constant Unsigned_8_Array_3D (1 .. 1, 1 .. 1, 1 .. 1) :=
                      (1 => (1 => (1 => 0)));
         begin
            return Dummy;
         end;
      end if;

   end Get_Picture;

   --  -------------------------------------------------------------------------

   function Set_All_Data (Yes_List, No_List : Integer_Matrix)
                       return Integer_Matrix is
      All_Data              : constant Integer_Matrix := Yes_List & No_List;
      All_Data_With_Offset  : Integer_Matrix (All_Data'Range,
                                              1 .. All_Data'Length (2) + 1);
   begin
      for row in All_Data_With_Offset'Range loop
         for col in All_Data_With_Offset'Range (2) loop
            if col <= All_Data'Length (2) then
               All_Data_With_Offset (row, col) := All_Data (row, col);
            elsif row <= Yes_List'Length then
               All_Data_With_Offset (row, col) := 1;
            else
               All_Data_With_Offset (row, col) := 0;
            end if;
         end loop;
      end loop;

      return All_Data_With_Offset;

   end Set_All_Data;

   --  -------------------------------------------------------------------------

   function To_2D (From : ML_U8_Types.Unsigned_8_Array_3D)
                   return Integer_Matrix is
      M2 : Integer_Matrix (1 .. From'Length * From'Length (2), From'Range (3));
   begin
      for row in From'Range loop
         for col in From'Range (2) loop
            for pix in From'Range (3) loop
               M2 ((row - 1) * From'Length (2) + col, pix) :=
                 Integer (From (row, col, pix));
            end loop;
         end loop;
      end loop;

      return M2;

   end To_2D;

   --  -------------------------------------------------------------------------

   function To_Boolean (From : Real_Float_Vector) return Boolean_Array is
      Bool : Boolean_Array (From'Range);
   begin
      for index in From'Range loop
         Bool (index) := From (index) > 0.0;
      end loop;

      return Bool;

   end To_Boolean;

   --  -------------------------------------------------------------------------

   function To_Picture (Flat_Data     : Integer_Matrix;
                        Height, Width : Positive; Weights : Real_Float_Vector)
                     return ML_U8_Types.Unsigned_8_Array_3D is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Support_5A.To_Picture ";
      Out_Data     : constant Boolean_Array :=
                       To_Boolean (To_Real_Float_Matrix (Flat_Data) * Weights);
      Result       : ML_U8_Types.Unsigned_8_Array_3D
        (1 .. Height, 1 .. Width, 1 .. 3);
   begin
      for row in Result'Range loop
         for col in Result'Range (2) loop
            for pix in Result'Range (3) loop
               if Out_Data ((row - 1) * Width + col) then
                  Result (row, col, pix) := 255;
               else
                  Result (row, col, pix) := 0;
               end if;
            end loop;
         end loop;
      end loop;

      return Result;

   end To_Picture;

   --  -------------------------------------------------------------------------

end Support_5A;
