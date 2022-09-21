
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Sets;
with Ada.Numerics.Elementary_Functions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Encode_Utils;
--  with Printing;
with Utilities;

package body Classifier_Utilities is

   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
   package Float_IO is new Ada.Text_IO.Float_IO (Num => Float);

   function Split_Raw_Data (Raw_Data    : Raw_Data_Vector;
                            Num_Outputs : Positive := 1)
                            return Multi_Output_Data_Record;

   --  ------------------------------------------------------------------------

   --  Arg_Max returns the index of the true values along an axis.
   function Arg_Max (Values : Boolean_Array) return Positive is
      Max_Value  : Boolean := False;
      Max_Index  : Positive := 1;
      Value      : Boolean;
   begin
      for index in Values'Range loop
         Value := Values (index);
         if Value and not Max_Value then
            Max_Index := index;
            Max_Value := Value;
         end if;
      end loop;

      return Max_Index;

   end Arg_Max;

   --  -----------------------------------------------------------------------
   --  Arg_Max returns the index of the true values along an axis.
   function Arg_Max (Values : Boolean_List) return Positive is
      Max_Value  : Boolean := False;
      Max_Index  : Positive := 1;
      Value      : Boolean;
   begin
      for index in Values.First_Index .. Values.Last_Index loop
         Value := Values.Element (index);
         if Value and not Max_Value then
            Max_Index := index;
            Max_Value := Value;
         end if;
      end loop;

      return Max_Index;

   end Arg_Max;

   --  -------------------------------------------------------------------------
   --  Arg_Max returns the indices of the maximum values along an axis.
   function Arg_Max (Values : Float_List) return Positive is
      Max_Value  : Float := Float'Safe_First;
      Max_Index  : Positive := 1;
      Value      : Float;
   begin
      for index in Values.First_Index .. Values.Last_Index loop
         Value := Values.Element (index);
         if Value > Max_Value then
            Max_Index := index;
            Max_Value := Value;
         end if;
      end loop;

      return Max_Index;

   end Arg_Max;

   --  ------------------------------------------------------------------------
   --  Arg_Max returns the index of the maximum value in a row for each row.
   function Arg_Max (Values : Real_Float_Matrix) return Integer_Array is
      --        Routine_Name : constant String :=
      --                           "Classifier_Utilities.Arg_Max Float_Matrix ";
      Max_Values  : Real_Float_Vector (Values'Range) :=
                      (others => Float'Safe_First);
      Value       : Float;
      Max_Indices : Integer_Array (Values'Range) := (others => 1);
   begin
      for row in Values'Range loop
         for col in Values'Range (2) loop
            Value := Values (row, col);
            if Value > Max_Values (row) then
               Max_Indices (row) := col;
               Max_Values (row) := Value;
            end if;
         end loop;

      end loop;

      return Max_Indices;

   end Arg_Max;

   --  ------------------------------------------------------------------------

   function Arg_Max (Values : Integer_List) return Positive is
      Max_Value  : Integer := Integer'First;
      Max_Index  : Positive := 1;
      Value      : Integer;
   begin
      for index in Values.First_Index .. Values.Last_Index loop
         Value := Values.Element (index);
         if Value > Max_Value then
            Max_Index := index;
            Max_Value := Value;
         end if;
      end loop;

      return Max_Index;

   end Arg_Max;

   --  -------------------------------------------------------------------------

   --  Bin_Count counts the number of occurrences of each value in Numbers.
   --  Each bin gives the number of occurrences of its index value in Numbers.
   function Bin_Count (Numbers : Natural_List) return Natural_List is
      use Ada.Containers;
      use Natural_Package;
      aNumber    : Natural := 0;
      Max_Number : Natural := 0;
      Bins       : Natural_List;
   begin
      for index in Numbers.First_Index .. Numbers.Last_Index loop
         aNumber := Numbers.Element (index);
         if aNumber > Max_Number then
            Max_Number := aNumber;
         end if;
      end loop;

      Bins.Set_Length (Count_Type (Max_Number + 1));
      for index in Bins.First_Index .. Bins.Last_Index loop
         Bins (index) := 0;
      end loop;

      for index in Numbers.First_Index .. Numbers.Last_Index loop
         aNumber := Numbers.Element (index);
         Bins (aNumber + 1) := Bins (aNumber + 1) + 1;
      end loop;

      return Bins;

   end Bin_Count;

   --  -------------------------------------------------------------------------

   function Bin_Count (Numbers : Value_Data_List)
                       return Natural_List is
      use Ada.Containers;
      use Natural_Package;
      aNumber    : Natural := 0;
      Max_Number : Natural := 0;
      Bins       : Natural_List;
   begin
      for index in Numbers.First_Index .. Numbers.Last_Index loop
         if Numbers.Element (index).Value_Kind /= Integer_Type then
            raise Value_Error with
              "Classifier_Utilities.Bin_Count called with non-integer data "
              & Data_Type'Image (Numbers.Element (index).Value_Kind);
         end if;

         aNumber := Numbers.Element (index).Integer_Value;
         if aNumber > Max_Number then
            Max_Number := aNumber;
         end if;
      end loop;

      Bins.Set_Length (Count_Type (Max_Number + 1));
      for index in Bins.First_Index .. Bins.Last_Index loop
         Bins (index) := 0;
      end loop;

      for index in Numbers.First_Index .. Numbers.Last_Index loop
         aNumber := Numbers.Element (index).Integer_Value;
         Bins (aNumber + 1) := Bins (aNumber + 1) + 1;
      end loop;

      return Bins;

   end Bin_Count;

   --  -------------------------------------------------------------------------

   function Compare_Float_Lists (L, R : Float_List) return Boolean is
      use Ada.Containers;
      use Float_Package;
      Diff     : Float := 0.0;
      Max_Diff : Float := 0.0;
      OK       : Boolean := R.Length = L.Length;
   begin
      if OK then
         for index in L.First_Index .. L.Last_Index loop
            Diff := abs (R.Element (index) - L.Element (index));
            OK := OK and then (Diff < 1.5 * 10.0 ** (-6));
            if Diff > Max_Diff then
               Max_Diff := Diff;
            end if;
         end loop;

         if not OK then
            New_Line;
            Put ("Classifier_Utilities.Compare_Float_Lists test ");
            Put_Line ("failed with error: " & Float'Image (Max_Diff));
            New_Line;
         end if;
      else
         New_Line;
         Put ("Classifier_Utilities.Compare_Float_Lists ");
         Put_Line ("test failed with different length lists, Left: "
                   & Count_Type'Image (L.Length) & ", Right: " &
                     Count_Type'Image (R.Length));
         New_Line;
      end if;

      return OK;
   end Compare_Float_Lists;

   --  -------------------------------------------------------------------------

   function Float_Precision (Number : Float; Precision : Natural)
                             return String is
      use Ada.Numerics.Elementary_Functions;
      Integer_Length : Positive;
      String_Length  : Positive;
   begin
      if abs Number < 1.0 then
         Integer_Length := 1;
      else
         Integer_Length :=
           1 + Integer (Float'Floor (Log (Float'Floor (abs Number), 10.0)));
      end if;

      String_Length := Integer_Length + Precision + 1;
      if Number < 0.0 then
         String_Length := String_Length + 1;
      end if;

      declare
         theString : String (1 .. String_Length);
      begin
         Float_IO.Put (To   => theString,
                       Item => Number,
                       Aft  => Precision,
                       Exp  => 0);
         return theString;
      end;

   end Float_Precision;

   --  -------------------------------------------------------------------------

   function Get_Column (List_2D      : Float_List_2D;
                        Column_Index : Positive) return Float_List is
      aList  : Float_List;
      Column : Float_List;
      Data   : Float;
   begin
      for index in List_2D.First_Index .. List_2D.Last_Index loop
         aList := List_2D.Element (index);
         Data := aList.Element (Column_Index);
         Column.Append (Data);
      end loop;

      return Column;

   end Get_Column;

   --  -------------------------------------------------------------------------

   function Init_Samples_Copy (Samples : Value_Data_Lists_2D)
                               return Value_Data_Lists_2D is
      Num_Samples    : constant Positive := Positive (Samples.Length);
      Sample_1       : constant Value_Data_List := Samples.Element (1);
      Num_Features   : constant Positive := Positive (Sample_1.Length);
      Boolean_Init   : Value_Record (Boolean_Type);
      Float_Init     : Value_Record (Float_Type);
      Integer_Init   : Value_Record (Integer_Type);
      UB_String_Init : Value_Record (UB_String_Type);
      Init_Features  : Value_Data_List;
      theCopy        : Value_Data_Lists_2D;
   begin
      for index in 1 .. Num_Features loop
         case Sample_1.Element (index).Value_Kind is
            when Boolean_Type =>
               Init_Features.Append (Boolean_Init);
            when Float_Type =>
               Init_Features.Append (Float_Init);
            when Integer_Type =>
               Init_Features.Append (Integer_Init);
            when UB_String_Type =>
               Init_Features.Append (UB_String_Init);
         end case;
      end loop;

      for index in 1 .. Num_Samples loop
         theCopy.Append (Init_Features);
      end loop;

      return theCopy;

   end Init_Samples_Copy;

   --  -------------------------------------------------------------------------

   function Load_Data (File_Name : String; Num_Outputs : Positive := 1)
                       return Multi_Output_Data_Record is
      Data_File    : File_Type;
      Raw_CSV_Data : Raw_Data_Vector;
      Output_Data  : Multi_Output_Data_Record;
   begin
      Open (Data_File, In_File, File_Name);
      Raw_CSV_Data := Utilities.Load_Raw_CSV_Data (Data_File);
      Close (Data_File);

      Output_Data := Split_Raw_Data (Raw_CSV_Data, Num_Outputs);
      return Output_Data;

   end Load_Data;

   --  ---------------------------------------------------------------------------

   function Max_Probability_Indices (Probabilities : Binary_Matrix)
                                     return Integer_Array is
      --        Routine_Name : constant String :=
      --                           "Classifier_Utilities.Max_Probability_Indices ";
      Max_Values  : Integer_Array (Probabilities'Range) := (others => 0);
      Value       : Integer;
      Max_Indices : Integer_Array (Probabilities'Range) := (others => 1);
   begin
      for row in Probabilities'Range loop
         for col in Probabilities'Range (2) loop
            Value := Probabilities (row, col);
            if Value > Max_Values (row) then
               Max_Indices (row) := col;
               Max_Values (row) := Value;
            end if;
         end loop;

      end loop;

      return Max_Indices;

   end Max_Probability_Indices;

   --  ------------------------------------------------------------------------

   function Max_Probability_Indices (Probabilities : Real_Float_Matrix)
                                     return Integer_Array is
      --        Routine_Name : constant String :=
      --                           "Classifier_Utilities.Max_Probability_Indices ";
      Max_Values  : Real_Float_Vector (Probabilities'Range) := (others => 0.0);
      Value       : Float;
      Max_Indices : Integer_Array (Probabilities'Range) := (others => 1);
   begin
      for row in Probabilities'Range loop
         for col in Probabilities'Range (2) loop
            Value := Probabilities (row, col);
            if Value > Max_Values (row) then
               Max_Indices (row) := col;
               Max_Values (row) := Value;
            end if;
         end loop;

      end loop;

      return Max_Indices;

   end Max_Probability_Indices;

   --  ------------------------------------------------------------------------

   procedure Parse_Header
     (Header       : Unbounded_List; Num_Features : Positive;
      Data_Record  : in out Multi_Output_Data_Record) is
   begin
      for index in 1 .. Positive (Header.Length) loop
         if index <= Num_Features then
            Data_Record.Feature_Names.Append (Header.Element (index));
         else
            Data_Record.Label_Names.Append (Header.Element (index));
         end if;
      end loop;

   end Parse_Header;

   --  -------------------------------------------------------------------------

   function Probabilities (aMatrix : Real_Float_Matrix)
                           return Real_Float_Matrix is
      --        Routine_Name : constant String :=
      --                           "Classifier_Utilities.Probabilities Matrix ";
      Sum    : Float;
      Result : Real_Float_Matrix (aMatrix'Range, aMatrix'Range (2));
   begin
      for row in aMatrix'Range loop
         Sum := 0.0;
         for col in aMatrix'Range (2) loop
            Sum := Sum + aMatrix (row, col);
         end loop;

         for col in aMatrix'Range (2) loop
            Result (row, col) := aMatrix (row, col) / Sum;
         end loop;
      end loop;

      return Result;

   end Probabilities;

   --  ------------------------------------------------------------------------

   function Probabilities (Vec : Real_Float_Vector) return Real_Float_Vector is
      --        Routine_Name : constant String :=
      --                           "Classifier_Utilities.Probabilities Vec";
      Sum    : Float := 0.0;
      Result : Real_Float_Vector (Vec'Range);
   begin
      for row in Vec'Range loop
         Sum := Sum + Vec (row);
      end loop;

      for row in Vec'Range loop
         Result (row) := Vec (row) / Sum;
      end loop;

      return Result;

   end Probabilities;

   --  ------------------------------------------------------------------------
   --  Row_Max_Indices returns the indices of the maximum value in each row of
   --  a matrix.
   function Row_Max_Indices (Values : Boolean_Matrix) return Natural_Array is
      Indices     : Natural_Array (1 .. Values'Length);
      Max_Value   : Boolean := False;
      Max_Index   : Positive;
      Col         : Natural;
   begin
      for row in Values'Range loop
         Max_Value := False;
         Max_Index := 1;
         Col := 0;
         while Col < Values'Last (2) and not Max_Value loop
            Col := Col + 1;
            if Values (row, Col) then
               Max_Index := Col;
               Max_Value := Values (row, col);
            end if;
         end loop;
         Indices (row) := Max_Index;
      end loop;

      return Indices;

   end Row_Max_Indices;

   --  -----------------------------------------------------------------------
   --  Row_Max_Indices returns the indices of the maximum value in each row
   --  of a matrix.
   function Row_Max_Indices (Values : Real_Float_Matrix) return Natural_Array is
      Indices   : Natural_Array (1 .. Values'Length);
      Max_Value : Float;
      Max_Index : Positive;
      Col       : Natural;
   begin
      for row in Values'Range loop
         Max_Value := Values (row, 1);
         Max_Index := 1;
         Col := 1;
         while Col < Values'Last (2) loop
            Col := Col + 1;
            if Values (row, Col) > Max_Value then
               Max_Index := Col;
               Max_Value := Values (row, col);
            end if;
         end loop;
         Indices (row) := Max_Index;
      end loop;

      return Indices;

   end Row_Max_Indices;

   --  -----------------------------------------------------------------------
   --  Search_Sorted_Integer_List finds the indices into List_A such that,
   --  if the corresponding elements in List_B were inserted before the indices,
   --  the order of List_A would be preserved.
   --  The Search_Sorted functions returns the indices where new elements in
   --  List_B should be inserted into List_A to keep List_A sorted.
   function Search_Sorted_Integer_List (List_A, List_B : Integer_List)
                                     return Integer_List is
      use Integer_Package;
      use Integer_Sorting;
      Item      : Integer;
      Pos_Found : Boolean;
      theList   : Integer_List;
   begin
      if not Is_Sorted (List_A) then
         raise Value_Error with
           "Search_Sorted called with unsorted list.";
      end if;

      for index_B in List_B.First_Index .. List_B.Last_Index loop
         Item := List_B.Element (index_B);
         Pos_Found := False;
         for index_A in List_A.First_Index .. List_A.Last_Index loop
            if not Pos_Found then
               Pos_Found := List_A (index_A) > Item;
               if Pos_Found then
                  theList.Append (Index_A);
               end if;
            end if;
         end loop;

         if not Pos_Found then
            theList.Append (List_A.Last_Index + 1);
         end if;
      end loop;

      return theList;

   end Search_Sorted_Integer_List;

   --  -------------------------------------------------------------------------

   function Search_Sorted_Float_List (List_A, List_B : Float_List)
                                   return Integer_List is
      use Float_Package;
      use Float_Sorting;
      Routine_Name : constant String :=
                       "Classifier_Utilities.Search_Sorted_Float_List ";
      Item         : Float;
      Pos_Found    : Boolean;
      theList      : Integer_List;
   begin
      if not Is_Sorted (List_A) then
         raise Value_Error with
         Routine_Name & "Search_Sorted called with unsorted list.";
      end if;

      for index_B in List_B.First_Index .. List_B.Last_Index loop
         Item := List_B.Element (index_B);
         Pos_Found := False;
         for index_A in List_A.First_Index .. List_A.Last_Index loop
            if not Pos_Found then
               Pos_Found := List_A (index_A) > Item;
               if Pos_Found then
                  theList.Append (Index_A);
               end if;
            end if;
         end loop;

         if not Pos_Found then
            theList.Append (List_A.Last_Index + 1);
         end if;
      end loop;

      return theList;

   end Search_Sorted_Float_List;

   --  -------------------------------------------------------------------------

   function Set_Diff (Values : Integer_Array; Uniques : Integer_Array)
                   return Natural_List is
      use Natural_Package;
      Unique_Vals : constant Integer_Array := Encode_Utils.Unique (Values);
      aVal        : Integer;
      U_Index     : Positive;
      Found       : Boolean;
      Diff        : Natural_List;
   begin
      for index in Unique_Vals'Range loop
         aVal := Unique_Vals (index);
         Found := False;
         U_Index := Uniques'First;
         while U_Index <= Uniques'Last and not Found loop
            Found := Uniques (U_Index) = aVal;
            U_Index := U_Index + 1;
         end loop;

         if Found then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;

   end Set_Diff;

   --  -------------------------------------------------------------------------

   function Set_Diff (Values : Integer_Array; Uniques : Natural_Array)
                   return Natural_List is
      use Natural_Package;
      Unique_Vals : constant Integer_Array := Encode_Utils.Unique (Values);
      aVal        : Natural;
      U_Index     : Positive;
      Found       : Boolean;
      Diff        : Natural_List;
   begin
      for index in Unique_Vals'Range loop
         aVal := Unique_Vals (index);
         Found := False;
         U_Index := Uniques'First;
         while U_Index <= Uniques'Last and not Found loop
            Found := Uniques (U_Index) = aVal;
            U_Index := U_Index + 1;
         end loop;
         if Found then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;
   end Set_Diff;

   --  -------------------------------------------------------------------------

   function Set_Diff (Values : Natural_Array; Uniques : Integer_Array)
                   return Natural_List is
      use Natural_Package;
      Unique_Vals : constant Natural_Array := Encode_Utils.Unique (Values);
      aVal        : Natural;
      U_Index     : Positive;
      Found       : Boolean;
      Diff        : Natural_List;
   begin
      for index in Unique_Vals'Range loop
         aVal := Unique_Vals (index);
         Found := False;
         U_Index := Uniques'First;
         while U_Index <= Uniques'Last and not Found loop
            Found := Uniques (U_Index) = aVal;
            U_Index := U_Index + 1;
         end loop;
         if Found then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;
   end Set_Diff;

   --  -------------------------------------------------------------------------

   --     function Set_Diff (Values : Boolean_Array; Uniques : Integer_Array)
   function Set_Diff (Values, Uniques : Boolean_Array) return Boolean_List is
      Unique_Vals : constant Boolean_Array := Encode_Utils.Unique (Values);
      aVal        : Boolean;
      U_Index     : Positive;
      Found       : Boolean;
      Diff        : Boolean_List;
   begin
      for index in Unique_Vals'Range loop
         aVal := Unique_Vals (index);
         Found := False;
         U_Index := Uniques'First;
         while U_Index <= Uniques'Last and not Found loop
            Found := Uniques (U_Index) = aVal;
            U_Index := U_Index + 1;
         end loop;
         if Found then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;
   end Set_Diff;

   --  -------------------------------------------------------------------------

   function Split_Raw_Data (Raw_Data    : Raw_Data_Vector;
                            Num_Outputs : Positive := 1)
                         return Multi_Output_Data_Record is
      use Ada.Containers;
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      --        Routine_Name   : constant String :=
      --                           "Classifier_Utilities.Split_Raw_Data ";
      aRow           : Unbounded_List := Raw_Data.First_Element;
      Num_Items      : constant Positive := Positive (aRow.Length);
      Num_Features   : constant Positive := Num_Items - Num_Outputs;
      Feature_Types  : Feature_Type_Array (1 .. Num_Features);
      Features_List  : Value_Data_Lists_2D;
      Label_Types    : Label_Type_Array  (1 .. Num_Outputs);
      Label_Values   : Value_Data_List;
      Labels_List    : Value_Data_Lists_2D;
      Feature_Values : Value_Data_List;
      Data           : Multi_Output_Data_Record;
   begin
      Parse_Header (aRow, Num_Features, Data);
      aRow := Raw_Data.Element (Positive'Succ (Raw_Data.First_Index));
      if aRow.Length > 1 then
         for f_index in 1 .. Num_Features loop
            declare
               Row_S     : constant String := To_String (aRow (f_index));
               S_Last    : constant Integer := Row_S'Last;
               Last_Char : constant Character := Row_S (S_Last);
            begin
               if Character'Pos (Last_Char) < 32 then
                  aRow (f_index) := To_Unbounded_String (Row_S (1 .. S_Last - 1));
               end if;
               Feature_Types (Positive (f_index)) :=
                 Utilities.Get_Data_Type (aRow (Positive (f_index)));
            end;
         end loop;

         for l_index in 1 .. Num_Outputs loop
            declare
               Row_S     : constant String := To_String (aRow (Num_Features + l_index));
               S_Last    : constant Integer := Row_S'Last;
               Last_Char : constant Character := Row_S (S_Last);
            begin
               if Character'Pos (Last_Char) < 32 then
                  aRow (Num_Features + l_index) := To_Unbounded_String (Row_S (1 .. S_Last - 1));
               end if;
               Label_Types (Positive (l_index)) :=
                 Utilities.Get_Data_Type (aRow (Positive (Num_Features + l_index)));
            end;
         end loop;

         for row_index in Positive'Succ (Raw_Data.First_Index) ..
           Raw_Data.Last_Index loop
            aRow := Raw_Data.Element (row_index);  --  Unbound list

            Feature_Values.Clear;
            for f_index in 1 .. Num_Features loop
               declare
                  Feat_String : constant String := To_String (aRow (f_index));
                  Value       : Value_Record (Feature_Types (Positive (f_index)));
               begin
                  case Feature_Types (Positive (f_index)) is
                  when Boolean_Type =>
                     Value.Boolean_Value := Boolean'Value (Feat_String);
                  when Integer_Type =>
                     Value.Integer_Value := Integer'Value (Feat_String);
                  when Float_Type =>
                     Value.Float_Value := Float'Value (Feat_String);
                  when UB_String_Type =>
                     Value.UB_String_Value := aRow (f_index);
                  end case;
                  Feature_Values.Append (Value);
               end;  --  declare block
            end loop;
            Features_List.Append (Feature_Values);

            for o_index in 1 .. Num_Outputs loop
               Label_Values.Clear;
               declare
                  Row_S     : constant String :=
                                To_String (aRow (Num_Features + o_index));
                  S_Last    : constant Integer := Row_S'Last;
                  Last_Char : constant Character := Row_S (S_Last);
               begin
                  if Character'Pos (Last_Char) < 32 then
                     aRow (Num_Features + o_index) :=
                       To_Unbounded_String (Row_S (1 .. S_Last - 1));
                  end if;
               end;

               declare
                  Label       : constant String :=
                                  To_String (aRow (Num_Features + o_index));
                  Label_Value : Value_Record (Label_Types (o_index));
               begin
                  case Label_Types (Positive (o_index)) is
                  when Boolean_Type =>
                     Label_Value.Boolean_Value := Boolean'Value (Label);
                  when Integer_Type =>
                     Label_Value.Integer_Value := Integer'Value (Label);
                  when Float_Type =>
                     Label_Value.Float_Value := Float'Value (Label);
                  when UB_String_Type =>
                     Label_Value.UB_String_Value :=
                       aRow (Num_Features + o_index);
                  end case;
                  Label_Values.Append (Label_Value);
               end;  --  declare block;
               Labels_List.Append (Label_Values);
            end loop;
         end loop;

         Data.Feature_Values := Features_List;
         Data.Label_Values := Labels_List;
      end if;

      return Data;

   end Split_Raw_Data;

   --  -----------------------------------------------------------------------

   function Sum_Cols (aList : Float_List_2D) return Float_List is
      theSum : Float_List;
      Value  : Float;
   begin
      for index in aList.First_Index .. aList.Last_Index loop
         Value := 0.0;
         for index_2 in aList.Element (1).First_Index ..
           aList.Element (1).Last_Index loop
            Value := Value + aList.Element (1).Element (index_2);
         end loop;
         theSum.Append (Value);
      end loop;

      return theSum;

   end Sum_Cols;

   --  -------------------------------------------------------------------------

   function Sum_Cols (aMatrix : Real_Float_Matrix) return Real_Float_Vector is
      theSum : Real_Float_Vector (aMatrix'Range);
      Value  : Float;
   begin
      for row in aMatrix'Range loop
         Value := 0.0;
         for col in aMatrix'Range (2) loop
            Value := Value + aMatrix (row, col);
         end loop;
         theSum (row) := Value;
      end loop;

      return theSum;

   end Sum_Cols;

   --  -------------------------------------------------------------------------

   function Sum_Cols (aList : Value_Data_Lists_2D) return Value_Data_List is
      theSum     : Value_Data_List;
      Value_Type : constant Data_Type :=
                     aList.Element (1).Element (1).Value_Kind;
      F_Value    : Float;
      I_Value    : Integer;
      Value_Rec  : Value_Record;
   begin
      for index in aList.First_Index .. aList.Last_Index loop
         F_Value := 0.0;
         I_Value := 0;
         Value_Rec := aList.Element (1).Element (1);
         case Value_Type is
         when Float_Type =>
            for index_2 in aList.Element (1).First_Index ..
              aList.Element (1).Last_Index loop
               F_Value := F_Value +
                 aList.Element (1).Element (index_2).Float_Value;
            end loop;
            Value_Rec.Float_Value := F_Value;

         when Integer_Type =>
            for index_2 in aList.Element (1).First_Index ..
              aList.Element (1).Last_Index loop
               I_Value := I_Value +
                 aList.Element (1).Element (index_2).Integer_Value;
            end loop;
            Value_Rec.Integer_Value := I_Value;

         when others => null;
         end case;
         theSum.Append (Value_Rec);
      end loop;

      return theSum;

   end Sum_Cols;

   --  -------------------------------------------------------------------------

   function To_Float_List (F : Value_Data_List) return Float_List is
      use Ada.Strings.Unbounded;
      Item   : Value_Record;
      Floats : Float_List;
   begin
      for index in F.First_Index .. F.Last_Index loop
         Item := F.Element (index);
         case Item.Value_Kind is
         when Boolean_Type =>
            if Item.Boolean_Value then
               Floats.Append (1.0);
            else
               Floats.Append (0.0);
            end if;
         when Float_Type =>
            Floats.Append (Item.Float_Value);
         when Integer_Type =>
            Floats.Append (Float (Item.Integer_Value));
         when UB_String_Type =>
            Floats.Append
              (Float (Integer'Value (To_String (Item.UB_String_Value))));
         end case;
      end loop;

      return Floats;

   end To_Float_List;

   --  -------------------------------------------------------------------------

   function To_Float_List (I : Integer_List) return Float_List is
      F_List : Float_List;
   begin
      for index in I.First_Index .. I.Last_Index loop
         F_List.Append (Float (I.Element (index)));
      end loop;

      return F_List;

   end To_Float_List;

   --  -------------------------------------------------------------------------

   function To_Float_List_2D (Data : Value_Data_Lists_2D)
                           return Float_List_2D is
      F2_List : Float_List_2D;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         F2_List.Append (To_Float_List (Data (index)));
      end loop;

      return F2_List;

   end To_Float_List_2D;

   --  -------------------------------------------------------------------------

   function To_Float_List_2D (I : Integer_List_2D) return Float_List_2D is
      F2_List : Float_List_2D;
   begin
      for index in I.First_Index .. I.Last_Index loop
         F2_List.Append (To_Float_List (I (index)));
      end loop;

      return F2_List;

   end To_Float_List_2D;

   --  -------------------------------------------------------------------------

   function To_Integer_List (Ints : Integer_Array) return Integer_List is
      Values : Integer_List;
   begin
      for index in Ints'Range loop
         Values.Append (Ints (index));
      end loop;

      return Values;

   end To_Integer_List;

   --  -------------------------------------------------------------------------

   function To_Integer_List (Ints : Value_Data_List) return Integer_List is
      use Ada.Strings.Unbounded;
      Item   : Value_Record;
      Values : Integer_List;
   begin
      for index in Ints.First_Index .. Ints.Last_Index loop
         Item := Ints.Element (index);
         case Item.Value_Kind is
         when Boolean_Type =>
            if Item.Boolean_Value then
               Values.Append (1);
            else
               Values.Append (0);
            end if;
         when Float_Type =>
            Values.Append (Integer (Item.Float_Value));
         when Integer_Type =>
            Values.Append (Item.Integer_Value);
         when UB_String_Type =>
            Values.Append
              (Integer'Value (To_String (Item.UB_String_Value)));
         end case;

      end loop;

      return Values;

   end To_Integer_List;

   --  -------------------------------------------------------------------------

   function To_Integer_Value_List (A : NL_Arrays_And_Matrices.Integer_Array)
                                return Value_Data_List is
      Data       : Value_Record (Integer_Type);
      A_List     : Value_Data_List;
   begin
      for index in A'First .. A'Last loop
         Data.Integer_Value := A (index);
         A_List.Append (Data);
      end loop;

      return A_List;
   end To_Integer_Value_List;

   --  -------------------------------------------------------------------------

   function To_Integer_Value_List_2D (A : NL_Arrays_And_Matrices.Integer_Array)
                                   return Value_Data_Lists_2D is
      Data       : Value_Record (Integer_Type);
      B_List     : Value_Data_List;
      Multi_List : Value_Data_Lists_2D;
   begin
      for index in A'First .. A'Last loop
         B_List.Clear;
         Data.Integer_Value := A (index);
         B_List.Append (Data);
         Multi_List.Append (B_List);
      end loop;

      return Multi_List;
   end To_Integer_Value_List_2D;

   --  -------------------------------------------------------------------------

   function To_Integer_List_2D (Data : Value_Data_Lists_2D)
                             return Integer_List_2D  is
      I2_List : Integer_List_2D;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         I2_List.Append (To_Integer_List (Data (index)));
      end loop;

      return I2_List;

   end To_Integer_List_2D;

   --  -------------------------------------------------------------------------

   function To_Multi_Value_List (A : NL_Arrays_And_Matrices.Multi_Value_Array)
                              return Value_Data_Lists_2D is
      Value    : Value_Record (Integer_Type);
      Row_List : Value_Data_Lists_2D;
      Col_List : Value_Data_List;
   begin
      for row in A'First .. A'Last loop
         Col_List.Clear;
         for col in A'Range (2) loop
            Value.Integer_Value := A (row, col);
            Col_List.Append (Value);
         end loop;
         Row_List.Append (Col_List);
      end loop;
      return Row_List;

   end To_Multi_Value_List;

   --  -------------------------------------------------------------------------

   function To_Natural_List (A : NL_Arrays_And_Matrices.Natural_Array)
                          return Natural_List is
      A_List : Natural_List;
   begin
      for index in A'First .. A'Last loop
         A_List.Append (A (index));
      end loop;
      return A_List;

   end To_Natural_List;

   --  -------------------------------------------------------------------------

   function To_Natural_List (Numbers : Value_Data_List)
                          return Natural_List is
      Item   : Value_Record;
      Values : Natural_List;
   begin
      for index in Numbers.First_Index .. Numbers.Last_Index loop
         Item := Numbers.Element (index);
         Assert (Item.Value_Kind = Integer_Type,
                 "Classifier_Utilities.To_Natural_List invalid item "
                 & Integer'Image (index) & " data type is " &
                   Data_Type'Image (Item.Value_Kind));
         Assert (Item.Integer_Value >= 0,
                 "Classifier_Utilities.To_Natural_List invalid value "
                 & Integer'Image (Item.Integer_Value) & " should be >= 0.");
         Values.Append (Natural (Item.Integer_Value));
      end loop;

      return Values;

   end To_Natural_List;

   --  -------------------------------------------------------------------------

   function To_Natural_Value_List (A : NL_Arrays_And_Matrices.Natural_Array)
                                return Value_Data_Lists_2D is
      Int_Array : NL_Arrays_And_Matrices.Integer_Array (1 .. A'Length);
   begin
      for index in A'First .. A'Last loop
         Int_Array (index) := A (index);
      end loop;
      return To_Integer_Value_List_2D (Int_Array);
   end To_Natural_Value_List;

   --  ------------------------------------------------------------------------

   function To_PL_Array (List_1D : Float_List; Num_Rows : Positive)
                      return Real_Float_Matrix is
      Routine_Name : constant String :=
                       "Classifier_Utilities.To_PL_Array ";
      Length_1D    : constant Positive := Positive (List_1D.Length);
      Num_Cols     : constant Positive := Length_1D / Num_Rows;
      End_Offset   : constant Positive := Num_Cols - 1;
      Start        : Positive := List_1D.First_Index;
      Result       : Real_Float_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
   begin
      Assert (Num_Rows * Num_Cols = Length_1D, Routine_Name & "Num_Rows" &
                Integer'Image (Num_Rows) & " is incompatible with List_1D size"
              & Integer'Image (Length_1D));

      for row in reverse 1 .. Num_Rows loop
         for col in Start .. Start + End_Offset loop
            Result (col - Start + 1, row) := Float (List_1D.Element (col));
         end loop;
         Start := Start + Num_Cols;
      end loop;

      return Result;

   end To_PL_Array;

   --  -------------------------------------------------------------------------

   function To_Value_2D_List (A : Value_Data_List)
                           return Value_Data_Lists_2D is
      Output_List : Value_Data_List;
      A2_List     : Value_Data_Lists_2D;
   begin
      A2_List.Clear;
      for index in A.First_Index .. A.Last_Index loop
         Output_List.Clear;
         Output_List.Append (A.Element (index));
         A2_List.Append (Output_List);
      end loop;

      return A2_List;

   end To_Value_2D_List;

   --  -------------------------------------------------------------------------

   function To_Value_2D_List (List_1D  : Value_Data_List;
                              Num_Rows : Positive)
                           return Value_Data_Lists_2D is
      Routine_Name : constant String :=
                       "Classifier_Utilities.To_Value_2D_List ";
      Length_1D    : constant Positive := Positive (List_1D.Length);
      Num_Cols     : constant Positive := Length_1D / Num_Rows;
      End_Offset   : constant Positive := Num_Cols - 1;
      Start        : Positive := List_1D.First_Index;
      Column_List  : Value_Data_List;
      List_2D      : Value_Data_Lists_2D;
   begin
      Assert (Num_Rows * Num_Cols = Length_1D, Routine_Name & "Num_Rows" &
                Integer'Image (Num_Rows) & " is incompatible with List_1D size"
              & Integer'Image (Length_1D));

      for index in 1 .. Num_Rows loop
         Column_List.Clear;
         for col in Start .. Start + End_Offset loop
            Column_List.Append (List_1D (col));
         end loop;
         List_2D.Append (Column_List);
         Start := Start + Num_Cols;
      end loop;

      return List_2D;

   end To_Value_2D_List;

   --  -------------------------------------------------------------------------

   function Transpose (Values : Value_Data_Lists_2D)
                    return  Value_Data_Lists_2D is
      use Ada.Containers;
      Num_Rows : constant Positive := Positive (Values.Length);
      Num_Cols : constant Count_Type := Values.Element (1).Length;
      In_Row   : Value_Data_List;
      Out_Row  : Value_Data_List;
      Result   : Value_Data_Lists_2D;
   begin
      Result.Set_Length (Num_Cols);
      for row in 1 .. Num_Rows loop
         In_Row := Values.Element (row);
         for index in In_Row.First_Index ..  In_Row.Last_Index loop
            Out_Row := Result.Element (index);
            Out_Row.Append (In_Row.Element (index));
            Result.Replace_Element (index, Out_Row);
         end loop;
      end loop;

      return Result;

   end Transpose;

   --  -------------------------------------------------------------------------

   function Unique (Nums : Integer_List)
                 return Integer_List is
      use Int_Sets;
      use Integer_Package;
      Unique_Set : Int_Sets.Set;
      Int_Curs   : Integer_Package.Cursor := Nums.First;
      Set_Curs   : Int_Sets.Cursor;
      Nums_List  : Integer_List;
   begin
      while Has_Element (Int_Curs) loop
         Unique_Set.Include (Element (Int_Curs));
         Next (Int_Curs);
      end loop;

      Set_Curs := Unique_Set.First;
      while Has_Element (Set_Curs) loop
         Nums_List.Append (Element (Set_Curs));
         Next (Set_Curs);
      end loop;
      return Nums_List;
   end Unique;

   --  -------------------------------------------------------------------------

end Classifier_Utilities;
