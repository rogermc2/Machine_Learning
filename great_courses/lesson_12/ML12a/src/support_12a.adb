
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;

package body Support_12A is

   function Means (Data : Float_Vector_List) return Real_Float_Vector;

   --  ------------------------------------------------------------------------

   function Arg_Min (Res2     : Real_Float_Matrix;
                     Min_Vals : out Real_Float_Vector) return Integer_Array is
      --        Routine_Name : constant String := "Support_11A.Arg_Min ";
      Min_Indices  : Integer_Array (Min_Vals'Range) := (others => 0);
      Min_Val      : Float;
      Min_Row      : Positive;
   begin
      for col in Res2'Range (2) loop
         Min_Val := Float'Safe_Last;
         for row in Res2'Range loop
            if Res2 (row, col) < Min_Val then
               Min_Val := Res2 (row, col);
               Min_Row := row;
            end if;
         end loop;
         Min_Vals (col) := Min_Val;
         Min_Indices (col) := Min_Row;
      end loop;

      return Min_Indices;

   end Arg_Min;

   --  -------------------------------------------------------------------------

   function Means (Data : Float_Vector_List) return Real_Float_Vector is
      use Real_Float_Arrays;
      aRow   : Real_Float_Vector (Data.Element (1)'Range);
      Result : Real_Float_Vector (Data.Element (1)'Range) := (others => 0.0);
   begin
      for row in Data.First_Index .. Data.Last_Index loop
         aRow := Data.Element (row);
         for col in aRow'Range loop
            Result (col) := Result (col) + aRow (col);
         end loop;
      end loop;

      return Result / Float (Data.Length);

   end Means;

   --  -------------------------------------------------------------------------
   --  Centre_Ids associate each sample with its closest cluster centre
   function Get_Plot_Data
     (Data         : Real_Float_Matrix; Cluster_Labels : Integer_Array;
      Center_IDs   : Integer_Array; Cluster_ID : Natural;
      Col_1, Col_2 : Positive) return Real_Float_Matrix is
      Routine_Name  : constant String := "Support_11A.Get_Plot_Data ";
      Selected_IDs  : ML_Types.Integer_List;
      Data_ID       : Positive;
      Label         : Natural;
      Selected_Row  : Real_Float_Vector (Data'Range (2));
      Selected_Cols : Real_Float_Vector (1 .. 2);
      Selected_Data : Float_Vector_List;
   begin
      --  Centre_Ids associates each sample (row) of Data with its closest
      --  cluster centre.
      --  for each Center_ID
      --    get the corresponding Cluster_Label
      --      if Cluster_Label is equal to the required Cluster_ID
      --        Add Center_ID to Selected_IDs list
      for sample in Center_IDs'Range loop
         Data_ID := Center_IDs (sample) + 1;
         Label := Cluster_Labels (Data_ID);
         if Label + 1 = Cluster_ID and then
           not Selected_IDs.Contains (sample) then
            Selected_IDs.Append (sample);
         end if;
      end loop;

      if Selected_IDs.Is_Empty then
         Put_Line (Routine_Name & "Selected_IDs list for cluster" &
                     Integer'Image (Cluster_ID) & " is empty.");
      else
         for index in Selected_IDs.First_Index .. Selected_IDs.Last_Index loop
            Selected_Row := Get_Row (Data, Selected_IDs (index));
            Selected_Cols (1) := Selected_Row (Col_1);
            Selected_Cols (2) := Selected_Row (Col_2);
            Selected_Data.Append (Selected_Cols);
         end loop;
      end if;

      return To_Real_Float_Matrix (Selected_Data);

   end Get_Plot_Data;

   --  -------------------------------------------------------------------------

   function Load_Data (File_Name : String) return ML_Types.Unbounded_List is
      Routine_Name : constant String := "Support_12A.Load_Data ";
      Data_File    : File_Type;
      Data         : ML_Types.Unbounded_List;
   begin
      Open (Data_File, In_File, File_Name);

      while not End_Of_File (Data_File) loop
         Data.Append (To_Unbounded_String (Get_Line (Data_File)));
      end loop;

      Close (Data_File);

      return Data;

   exception
      when others =>
         Put_Line (Routine_Name & "failed.");
         return Data;

   end Load_Data;

   --  -------------------------------------------------------------------------

   function Select_Items (Data  : Integer_Matrix; Center_IDs : Integer_Array;
                          Index : Natural) return ML_Types.Integer_List is
      Items : ML_Types.Integer_List;
   begin
      for lab_index in Center_IDs'Range loop
         if Center_IDs (lab_index) = Index then
            Items.Append (Data (lab_index, 1));
         end if;
      end loop;

      return Items;

   end Select_Items;

   --  -------------------------------------------------------------------------

end Support_12A;
